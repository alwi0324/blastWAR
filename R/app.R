variasiSalam <- function() {
  salam <- c("Assalamu'alaikum, ", "Semangat Pagi! ", "Yth. ", "Halo, ", "Selamat Pagi, ", "Hai, ")
  sample(salam, 1)
}

variasiPenutup <- function() {
  tutup <- c("Terima Kasih! Semoga sukses ☺",
             "Sukses selalu untuk Anda!",
             "Semoga berhasil, Terima Kasih.",
             "Terima kasih dan semangat!")
  sample(tutup, 1)
}

buatPesan <- function(tujuan, tmpl) {
  buka <- variasiSalam()

  if (buka == "Assalamu'alaikum, ") {
    tutup <- "Wassalamu'alaikum."
  } else {
    tutup <- variasiPenutup()
  }

  adaSalam <- gsub("\\{salam\\}", buka, tmpl)
  adaNama <- gsub("\\{nama\\}", tujuan, adaSalam)
  adaPenutup <- gsub("\\{penutup\\}", tutup, adaNama)

  return(adaPenutup)
}

#' @import httr
#' @noRd
cek_nomor_wa <- function(nomor, token) {
  tryCatch({
    res <- POST(
      url = "https://api.fonnte.com/validate",
      add_headers("Authorization" = token),
      body = list(target = nomor),
      encode = "form"
    )
    hasil <- content(res, "parsed", simplifyVector = TRUE)
    if(is.null(hasil$registered)) return(FALSE)
    return(nomor %in% unlist(hasil$registered))
  }, error = function(e) {
    return(FALSE)
  })
}

#' @import httr
#' @noRd
kirim_fonnte <- function(phone, message, token) {
  tryCatch({
    res <- POST(
      url = "https://api.fonnte.com/send",
      add_headers("Authorization" = token),
      body = list(target = phone, message = message),
      encode = "multipart"
    )
    return(content(res, "parsed"))
  }, error = function(e) {
    return(list(status = FALSE, reason = "API Error/Connection issue"))
  })
}

#' @import shiny
#' @import shinyjs
#' @import shinythemes
#' @noRd
ui <- fluidPage(
  theme = shinytheme("flatly"),
  useShinyjs(), # Mengaktifkan shinyjs

  titlePanel("WhatsApp Blast Tool (Fonnte & Google Sheets)"),

  sidebarLayout(
    sidebarPanel(
      h4("Konfigurasi"),
      passwordInput("token", "Fonnte Token:", placeholder = "Masukkan Token Fonnte di sini"),
      textInput("sheet_url", "Link Google Sheet:", placeholder = "Masukkan Link Google Sheet di sini"),

      hr(),
      actionButton("btn_load", "Load Data", icon = icon("table"), class = "btn-info btn-block"),
      br(),
      actionButton("btn_blast", "Mulai Blast WA", icon = icon("paper-plane"), class = "btn-danger btn-block"),

      br(),
      helpText(
        HTML("Catatan:<br>
        1. Pastikan kolom di Sheet1 bernama 'Nama' dan 'telp'<br>
        2. Pastikan ada sheet yang bernama 'Template' dan memiliki kolom bernama 'template'<br>
        3. Gunakan format internasional pada kolom telp (untuk Indonesia berawalan '62' tanpa tanda '+')")
      )
    ),

    mainPanel(
      tabsetPanel(id = "main_tabs", # Memberi ID pada tabsetPanel
                  tabPanel("Data Preview", value = "tab_preview",
                           br(),
                           DTOutput("tbl_data")
                  ),
                  tabPanel("Log Proses", value = "tab_log", # Memberi ID pada Tab Log
                           br(),
                           verbatimTextOutput("log_output"),
                           tags$head(tags$style("#log_output{height:400px; overflow-y:scroll; font-size: 12px;}"))
                  ),
                  tabPanel("Laporan Gagal", value = "tab_error", # Memberi ID pada Tab Error
                           br(),
                           DTOutput("tbl_error")
                  )
      )
    )
  )
)

#' @import dplyr
#' @import shiny
#' @import shinyjs
#' @import googlesheets4
#' @import DT
#' @noRd
server <- function(input, output, session) {

  # Reactive values untuk menyimpan state
  rv <- reactiveValues(
    data = NULL,
    logs = "Siap memproses data...\n",
    errors = data.frame(Nama=character(), telp=character(), reason=character()),
    tmpl = NULL
  )

  # Fungsi helper untuk update log di UI
  add_log <- function(msg) {
    # Pastikan rv$logs adalah karakter sebelum concatenating
    rv$logs <- paste0(as.character(rv$logs), msg, "\n")
  }

  # --- INISIALISASI & KONTROL TOMBOL ---

  # Nonaktifkan tombol Load Data dan Blast, dan sembunyikan tab saat aplikasi dimuat
  disable("btn_load")
  disable("btn_blast")

  # Sembunyikan tab dengan delay singkat (1 detik)
  shinyjs::runjs(
    'setTimeout(function(){
       Shiny.setInputValue("init_delay", true);
       $("#main_tabs li a[data-value=\'tab_log\']").parent().hide();
       $("#main_tabs li a[data-value=\'tab_error\']").parent().hide();
    }, 1000);'
  )


  # Observer untuk mengontrol tombol "Load Data" (tergantung URL)
  observe({
    sheet_url_filled <- !is.null(input$sheet_url) && input$sheet_url != ""

    if (sheet_url_filled) {
      enable("btn_load")
    } else {
      disable("btn_load")
      # Reset status data jika URL dihapus
      rv$data <- NULL
    }
  })

  # Observer untuk mengontrol tombol "Mulai Blast WA" (tergantung Token & Data)
  observe({
    is_valid_inputs <- !is.null(input$token) && input$token != "" &&
      !is.null(input$sheet_url) && input$sheet_url != ""

    is_data_loaded <- !is.null(rv$data)

    if (is_valid_inputs && is_data_loaded) {
      enable("btn_blast")
    } else {
      disable("btn_blast")
    }
  })

  # 1. Load Data dari Google Sheets
  observeEvent(input$btn_load, {
    req(input$sheet_url)

    withProgress(message = 'Mengambil data dari Google Sheets...', value = 0, {
      tryCatch({
        sheetku <- gs4_get(input$sheet_url)

        # Validasi template
        if (!all("Template" %in% sheetku$sheets$name)) {
          showNotification("Error: Pastikan ada sheet dengan nama 'Template' dan kolomnya bernama 'template'", type = "error")
          rv$data <- NULL
          rv$tmpl <- NULL
        } else {
          df <- read_sheet(sheetku, sheet = "Sheet1") %>% as.data.frame()
          rv$tmpl <- read_sheet(sheetku, sheet = "Template") %>% as.character() # template pesan dari google sheet

          # Validasi kolom
          if(!all(c("Nama", "telp") %in% colnames(df))) {
            showNotification("Error: Kolom 'Nama' atau 'telp' tidak ditemukan di Sheet1!", type = "error")
            rv$data <- NULL
          } else {
            rv$data <- df
            add_log(paste0("✅ Berhasil load ", nrow(df), " baris data."))
            showNotification("Data berhasil dimuat!", type = "message")
          }

        }

      }, error = function(e) {
        add_log(paste0("❌ Gagal load data: ", e$message))
        showNotification("Gagal mengambil data. Cek Log.", type = "error")
        rv$data <- NULL
        rv$tmpl <- NULL
      })
    })
  })

  # Output Tabel Data Preview
  output$tbl_data <- renderDT({
    req(rv$data)
    datatable(rv$data, options = list(pageLength = 10))
  })

  # Output Log
  output$log_output <- renderText({
    rv$logs
  })

  # Output Tabel Error
  output$tbl_error <- renderDT({
    datatable(rv$errors, options = list(pageLength = 10))
  })

  # 2. Proses Blast WA
  observeEvent(input$btn_blast, {
    req(rv$data, input$token)

    hide("btn_load")
    hide("btn_blast")

    # Reset Log dan Error
    add_log(paste0("--- Blast WA dimulai pada ", unlist(strsplit(as.character(Sys.time()), "\\."))[1], " ---\n"))
    rv$errors <- data.frame(Nama=character(), telp=character(), reason=character())

    data_blast <- rv$data
    total <- nrow(data_blast)
    fonnte_token <- input$token
    suksesCount <- 0

    withProgress(message = 'Sedang Blast WA...', value = 0, {

      for (i in 1:total) {
        nama_kontak <- data_blast$Nama[i]
        telp_kontak <- as.character(data_blast$telp[i])

        # Update Progress Bar
        incProgress(i/total, detail = paste0("[",i,"/",total,"] Memproses: ", nama_kontak))
        add_log(paste0("--- [", i, "/", total, "] Memproses: ", nama_kontak, " (", telp_kontak, ") ---"))

        # 1. Cek Validitas Nomor (Delay kecil untuk API check)
        isWaValid <- cek_nomor_wa(telp_kontak, fonnte_token)
        Sys.sleep(0.5)

        if (isWaValid) {
          msg <- buatPesan(nama_kontak, rv$tmpl)
          resp <- kirim_fonnte(telp_kontak, msg, fonnte_token)

          if (isFALSE(resp$status) || is.null(resp$status)) {
            reason <- ifelse(is.null(resp$reason), "Unknown Error", resp$reason)
            add_log(paste0("❌ Gagal kirim: ", reason))
            # Gunakan rbind.data.frame untuk menghindari error row.names
            rv$errors <- rbind.data.frame(rv$errors, data.frame(Nama=nama_kontak, telp=telp_kontak, reason=reason))
          } else {
            add_log("✅ Pesan TERKIRIM.")
            suksesCount <- suksesCount + 1

            # Delay Random (Hanya jika bukan pesan terakhir)
            if (i < total) {
              waitTime <- sample(10:60, 1)
              add_log(paste0("⏳ Menunggu ", waitTime, " detik...\n"))

              # Tentukan langkah progress bar per iterasi
              total_steps <- total # Total langkah yang sudah dihitung di withProgress
              progress_increment_per_second <- (1 / total_steps) / waitTime

              for (s in 1:waitTime) {

                # Tetapkan detail progress bar agar berubah setiap detik
                incProgress(
                  amount = progress_increment_per_second,
                  detail = paste0("\n⏳ ", waitTime - s, " detik tersisa sebelum dikirim ke ", data_blast$Nama[i+1])
                )

                # Jeda 1 detik
                Sys.sleep(1)
              }

              # Setelah loop jeda selesai, kembalikan detail progress bar ke "Memproses"
              incProgress(amount = 0, detail = "Selesai menunggu, melanjutkan...")
            }
          }

        } else {
          add_log("❌ Nomor TIDAK TERDAFTAR di WA.\n")
          rv$errors <- rbind.data.frame(rv$errors, data.frame(Nama=nama_kontak, telp=telp_kontak, reason="Not registered"))
        }

        # Jeda tambahan setelah setiap 30 pesan (Super-Delay, Opsional)
        if (i < total && i %% 30 == 0) {
          super_delay <- sample(300:600, 1) # Jeda 5-10 menit
          add_log(paste0("\n--- BREAK PANJANG ---"))
          add_log(paste0("💤 Jeda 30 pesan tercapai. Menunggu ", super_delay, " detik."))
          Sys.sleep(1) # Jeda UI update
          Sys.sleep(super_delay)
          add_log("--- BREAK SELESAI. Lanjut kirim. ---\n")
        }
      }
    })

    add_log(paste0("\n--- Proses Blast selesai pada ", unlist(strsplit(as.character(Sys.time()), "\\."))[1], " ---"))
    add_log(paste0("🏁 Selesai! Berhasil terkirim ke ", suksesCount, " dari ", total, " kontak."))
    showNotification("Blast Selesai!", type = "message", duration = 8)

    # --- KONTROL TAMPILAN TAB ---
    # Tampilkan tab Log Proses dan Error Report saat blast selesai
    show(selector = '#main_tabs li a[data-value="tab_log"]')
    show(selector = '#main_tabs li a[data-value="tab_error"]')
    updateTabsetPanel(session, "main_tabs", selected = "tab_log") # Pindah ke tab Log
  })
}

#' @title Blast WhatsApp Messages to Multiple Number
#' @description Blast WhatsApp messages to multiple Number using template from Google Sheets.
#' @import shiny
#' @export
#' @examples
#' \dontrun{
#' # Make sure your Fonnte token is valid and Google Sheet has sheets named "Sheet1" and "Template"
#' # blastWA()
#' }
blastWA <- function() {
  shinyApp(ui = ui, server = server)
}
