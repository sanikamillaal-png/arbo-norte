# Sala de Situação - Região de Saúde Norte
# -----------------------------------------------------------------------------
# Pacotes necessários
# -----------------------------------------------------------------------------
required <- c("shiny", "bslib", "htmltools", "markdown", "rmarkdown", "DT", "leaflet", "readxl","nominatimlite", "geosphere")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) {
  stop("Instale os pacotes: ", paste(missing, collapse = ", "))
}
library(shiny)
library(bslib)
library(htmltools)
library(DT)
library(leaflet)
library(readxl)
library(nominatimlite)
library(geosphere)
library(markdown)
# -----------------------------------------------------------------------------
# Arquivos auxiliares gerados automaticamente
# -----------------------------------------------------------------------------

dir.create("www", showWarnings = FALSE)
dir.create("rmd", showWarnings = FALSE)

css <- "
:root { --navy:#102f63; --navy2:#173f7a; --gold:#d9bd70; --ink:#162033; --muted:#5d6879; --bg:#f5f7fb; --line:#dce3ee; }
body { background:var(--bg); color:var(--ink); font-family:Segoe UI,Arial,sans-serif; }
.navbar { background:var(--navy)!important; border-bottom:4px solid var(--gold)!important; }
.navbar-brand, .navbar-nav>li>a { color:#fff!important; font-weight:600; }
.navbar-nav>li>a:hover, .navbar-nav>li.active>a { background:var(--navy2)!important; color:#fff!important; }
.hero { background:linear-gradient(135deg,var(--navy),var(--navy2)); color:#fff; border:4px solid var(--gold); border-radius:18px; padding:26px; margin:16px 0 26px; display:flex; align-items:center; gap:24px; flex-wrap:wrap; }
.hero img { width:170px; height:170px; object-fit:contain; border-radius:50%; border:4px solid var(--gold); background:#fff; }
.hero h1 { color:#fff; font-size:clamp(1.8rem,4vw,3rem); margin:0; line-height:1.06; }
.hero p { margin:12px 0 0; color:#eef3fb; font-size:1.08rem; }
.card-grid { display:grid; grid-template-columns:repeat(auto-fit,minmax(220px,1fr)); gap:16px; margin:18px 0 26px; }
.card { background:#fff; border:1px solid var(--line); border-radius:14px; padding:18px; box-shadow:0 3px 14px rgba(16,47,99,.08); height:100%; }
.card h3 { margin-top:0; color:var(--navy); font-size:1.12rem; }
.quick { display:flex; flex-wrap:wrap; gap:10px; margin:18px 0 26px; }
.quick a { background:var(--navy); border:2px solid var(--gold); border-radius:999px; color:#fff!important; padding:11px 16px; text-decoration:none!important; font-weight:600; }
.quick a:hover { background:var(--navy2); }
.section-note { background:#fff8e6; border-left:5px solid var(--gold); border-radius:9px; padding:14px 16px; margin:16px 0; }
.alert-danger { border-left:5px solid #b3261e; border-radius:9px; }
.alarm { border-left:5px solid var(--gold); font-weight:600; }
.flow { text-align:center; background:#fff; border:1px solid var(--line); border-radius:14px; padding:20px; }
.flow-step { display:inline-block; padding:12px 18px; margin:5px; border-left:5px solid var(--navy); border-radius:9px; background:#fff; box-shadow:0 2px 8px rgba(0,0,0,.06); font-weight:600; }
.flow-arrow { color:var(--gold); font-size:1.8rem; }
.btn-primary { background:var(--navy)!important; border-color:var(--navy)!important; }
.btn-primary:hover { background:var(--navy2)!important; }
.table>thead>tr>th { background:var(--navy); color:#fff; }
footer { color:var(--muted); border-top:1px solid var(--line); margin-top:30px; padding:16px 0; font-size:.9rem; }
.small-muted { color:var(--muted); font-size:.9rem; }
.exame-collapse {
  background: #fff;
  border: 1px solid var(--line);
  border-radius: 12px;
  margin: 12px 0;
  padding: 0;
  box-shadow: 0 2px 8px rgba(16,47,99,.06);
}

.exame-collapse summary {
  cursor: pointer;
  color: var(--navy);
  font-size: 1.12rem;
  font-weight: 700;
  padding: 16px 18px;
  list-style-position: inside;
}

.exame-collapse summary:hover {
  background: #f1f5fb;
  border-radius: 12px;
}

.exame-collapse[open] summary {
  border-bottom: 1px solid var(--line);
}

.exame-content {
  padding: 16px 18px;
}
"
writeLines(css, "www/styles.css")

# Conteúdo R Markdown. Este arquivo serve como página documental do site.
#rmd_text <- '-- title: "Guia rápido de arboviroses"\noutput:\n  html_document:\n    self_contained: false\n    toc: false\n---\n\n## Como usar este guia\n\nUse os menus do site para consultar avaliação inicial, sinais de alarme, exames e rede local. O conteúdo clínico deve ser validado e atualizado pela área técnica responsável antes do uso institucional.\n\n## Fontes a cadastrar\n\n- Manual do Ministério da Saúde para diagnóstico e manejo clínico da dengue.\n- Plano de Enfrentamento da Dengue e Outras Arboviroses da SES-DF.\n- Notas técnicas, POPs e fluxos vigentes da SES-DF, LACEN e Região Norte.\n'
#writeLines(rmd_text, "rmd/guia_arboviroses.Rmd")

# -----------------------------------------------------------------------------
# Dados iniciais
# -----------------------------------------------------------------------------
rede_norte <- data.frame(
  Unidade = c("UBS da Região Norte", "Hospital Regional de Planaltina", "Hospital Regional de Sobradinho", "UPA de Sobradinho II", "UPA de Planaltina"),
  Territorio = c("Arapoanga, Planaltina, Sobradinho, Sobradinho II e Fercal", "Planaltina", "Sobradinho", "Confirmar conforme endereço e fluxo vigente","Confirmar conforme endereço e fluxo vigente"),
  Funcao = c("Acolhimento e acompanhamento conforme protocolo", "Avaliação de maior complexidade", "Avaliação de maior complexidade", "Urgência e emergência", "Urgência e emergência"),
  Contato = c("Inserir contato institucional", "Inserir contato institucional", "Inserir contato institucional", "Inserir contato institucional", "Inserir contato institucional"),
  stringsAsFactors = FALSE
)

exames <- data.frame(
  Agravo = c("Dengue", "Dengue", "Chikungunya"),
  Exame = c("NS1 / RT-PCR", "IgM", "Exame específico conforme nota técnica vigente"),
  Momento = c("Conforme janela oficial", "Conforme janela oficial", "Conforme janela oficial"),
  Amostra = c("Confirmar fluxo local", "Confirmar fluxo local", "Confirmar fluxo local"),
  Observacao = c("Não atrasar a conduta clínica", "Interpretar com data de sintomas", "Validar com LACEN/SES-DF"),
  stringsAsFactors = FALSE
)
# -----------------------------------------------------------------------------
# Bloco de fluxos do LACEN
# -----------------------------------------------------------------------------
fluxos_lacen <- data.frame(
  Agravo = c(
    "Dengue",
    "Chikungunya"
  ),
  
  Exame = c(
    "Dengue PCR",
    "Chikungunya Sorologia IgM"
  ),
  
  Metodo = c(
    "RT-PCR em tempo real",
    "MAC-ELISA (IgM)"
  ),
  
  Periodo_coleta = c(
    "1º ao 5º dia de início dos sintomas",
    "A partir do 6º dia de início dos sintomas"
  ),
  
  Amostra = c(
    "Soro em tubo com gel separador",
    "Soro em tubo com gel separador"
  ),
  
  Volume_minimo = c(
    "2 mL",
    "2 mL"
  ),
  
  Acondicionamento = c(
    "2 °C a 8 °C até 48 horas; depois, conforme orientação laboratorial",
    "Refrigerado entre 2 °C e 8 °C"
  ),
  
  Envio = c(
    "Até 48 horas",
    "Até 48 horas"
  ),
  
  Documentacao = c(
    "Ficha SINAN",
    "Ficha SINAN devidamente preenchida"
  ),
  
  stringsAsFactors = FALSE
)
# -----------------------------------------------------------------------------
# Coordenadas das unidades da Rede Norte
# -----------------------------------------------------------------------------

coordenadas_rede <- read_excel(
  "ubs_gsap_coordenadas.xlsx",
  sheet = 1
)

coordenadas_rede <- as.data.frame(coordenadas_rede)

names(coordenadas_rede) <- c(
  "Gestao",
  "Territorio",
  "Unidade",
  "Latitude",
  "Longitude"
)

coordenadas_rede$Latitude <- as.numeric(coordenadas_rede$Latitude)
coordenadas_rede$Longitude <- as.numeric(coordenadas_rede$Longitude)
message("Planilha carregada com sucesso.")
message("Número de linhas: ", nrow(coordenadas_rede))
message(
  "Colunas: ",
  paste(names(coordenadas_rede), collapse = ", ")
)
# -----------------------------------------------------------------------------
# Dados da Rede Norte
# -----------------------------------------------------------------------------

rede_norte <- data.frame(
  Unidade = c(
    "UBS da Região Norte",
    "Hospital Regional de Planaltina",
    "Hospital Regional de Sobradinho",
    "UPA de Sobradinho II",
    "UPA de Planaltina"
  ),
  
  Territorio = c(
    "Arapoanga, Planaltina, Sobradinho, Sobradinho II e Fercal",
    "Planaltina",
    "Sobradinho",
    "Sobradinho II",
    "Planaltina"
  ),
  
  Funcao = c(
    "Acolhimento e acompanhamento conforme protocolo",
    "Avaliação de maior complexidade",
    "Avaliação de maior complexidade",
    "Urgência e emergência",
    "Urgência e emergência"
  ),
  
  Contato = c(
    "Inserir contato institucional",
    "Inserir contato institucional",
    "Inserir contato institucional",
    "Inserir contato institucional",
    "Inserir contato institucional"
  ),
  
  stringsAsFactors = FALSE
)
# -----------------------------------------------------------------------------
# Interface
# -----------------------------------------------------------------------------
ui <- navbarPage(
  title = "Sala de Situação Região de Saúde Norte",
  id = "nav",
  header = tagList(
    tags$head(
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "styles.css"
      )
    )
  ),
  # -----------------------------------------------------------------------------
  # Aba de início
  # -----------------------------------------------------------------------------

  tabPanel(
    "Início",
    value = "inicio",
    
    div(
      class = "container-fluid",
# -------------------------------------------------------------------------
# Logo e identificação da Sala de Situação
# -------------------------------------------------------------------------
      div(
        class = "hero",
        
        img(
          src = "logo-sala-situacao-norte.png", 
          alt = "Logo Sala de Situação Região de Saúde Norte"
        ),
        
        div(
          h1("SALA DE SITUAÇÃO REGIÃO DE SAÚDE NORTE"),
          h3("Apoio prático à Atenção Primária para dengue e chikungunya"),
          p("Aviso institucional:  ferramenta de apoio. Não substitui avaliação profissional,protocolos oficiais ou fluxos vigentes.")
        ),
        
      ),
      
# -------------------------------------------------------------------------
# Alerta de notificação compulsória
# -------------------------------------------------------------------------
      div(
        class = "alert alert-danger",
        role = "alert",
        style = "font-size: 1.05rem; font-weight: 600; margin-top: 10px;",
        
        HTML("⚠️ <strong>NOTIFICAÇÃO COMPULSÓRIA</strong>"),
        
        br(),
        
        "Notifique todo caso suspeito de dengue ou chikungunya, ",
        "conforme a ficha, o sistema e os prazos definidos no fluxo vigente.",
        
        br(),
        br(),
        
        a(
          class = "btn btn-danger",
          href = "https://www.saude.df.gov.br/notificacao-compulsoria",
          target = "_blank",
          "Acessar orientações de notificação"
        )
      ),
      
      
# -------------------------------------------------------------------------
# Cards informativos
# -------------------------------------------------------------------------
      div(
        class = "card-grid",
        
        div(
          class = "card",
          
          strong("Avaliar paciente"),
          
          p(
            "Acesse rapidamente os sintomas, o dia de doença ",
            "e os sinais de alarme."
          )
        ),
        
        div(
          class = "card",
          
          strong("Diferenciar arboviroses"),
          
          p(
            "Compare dengue e chikungunya e consulte os ",
            "principais aspectos clínicos."
          )
        ),
        
        div(
          class = "card",
          
          strong("Consultar a Rede de saúde"),
          
          p(
            "Localize informações sobre unidades, referências ",
            "e fluxos assistenciais."
          )
        ),
        
        div(
          class = "card",
          
          strong("Conteúdo atualizado"),
          
          p(
            "Consulte documentos oficiais, notas técnicas, POPs ",
            "e referências utilizadas no painel."
          )
        )
      ),
      
# -------------------------------------------------------------------------
# Sinais de alarme
# -------------------------------------------------------------------------
      div(
        class = "section-note",
        
        strong("Sinais de alarme e protocolos oficiais"),
        
        p(
          "Na presença de qualquer um destes sinais, reavalie o paciente ",
          "imediatamente e consulte o protocolo específico do agravo. ",
          "Dengue e chikungunya possuem fluxos de manejo próprios."
        ),
        
        tags$ul(
          tags$li("Dor abdominal intensa e contínua."),
          tags$li("Vômitos persistentes."),
          tags$li("Acúmulo de líquidos."),
          tags$li("Lipotimia ou hipotensão postural."),
          tags$li("Hepatomegalia."),
          tags$li("Sangramento de mucosa."),
          tags$li("Letargia ou irritabilidade.")
        )
      ),
      
      tags$br(),
      
# -------------------------------------------------------------------------
# Links para documentos oficiais
# -------------------------------------------------------------------------
      div(
        class = "button-row",
        
        a(
          class = "btn btn-primary",
          href = "https://www.gov.br/saude/pt-br/composicao/svsa/resposta-a-emergencias/coes/arboviroses/publicacoes/fluxograma-do-manejo-clinico-da-dengue.pdf/view",
          target = "_blank",
          "Fluxograma oficial: dengue"
        ),
        
        a(
          class = "btn btn-primary",
          href = "https://www.gov.br/saude/pt-br/centrais-de-conteudo/publicacoes/guias-e-manuais/2024/guia-chikungunya-manejo-clinico-2o-edicao.pdf",
          target = "_blank",
          "Guia oficial: chikungunya"
        )
      ),
      
# -------------------------------------------------------------------------
# Rodapé
# -------------------------------------------------------------------------
      tags$footer(
        
        "Elaborado por Kamilla Araújo — Sanitarista residente em Vigilância em Saúde — UnB/HUB para a Sala de Situação da Região de Saúde Norte.",
        
        br(),
        
        "Versão 1.0 | Última atualização: 12 de agosto de 2026 | Conteúdo sujeito à validação e atualização técnica."
      )
    )
  ),
# -------------------------------------------------------------------------
# Aba de avaliação do paciente
# -------------------------------------------------------------------------

  tabPanel(
    "Avaliar paciente",
    value = "avaliar",
    
    div(
      class = "container-fluid",
      
      div(
        class = "alert alert-danger",
        role = "alert",
        style = "font-size: 1.05rem; font-weight: 600; margin-top: 10px;",
        
        HTML("⚠️ <strong>NOTIFICAÇÃO COMPULSÓRIA</strong>"),
        
        br(),
        
        "Notifique todo caso suspeito de dengue ou chikungunya, ",
        "conforme a ficha, o sistema e os prazos definidos no fluxo vigente.",
        
        br(),
        br(),
        
        a(
          class = "btn btn-danger",
          href = "https://www.saude.df.gov.br/notificacao-compulsoria",
          target = "_blank",
          "Acessar orientações de notificação"
        )
        
      ),
      h2("Avaliação rápida"),
      div(class = "section-note", "Use o resultado como apoio à consulta do protocolo. A classificação final depende da avaliação clínica e dos fluxos vigentes."),
      fluidRow(
        
        column(
          4,
          
          selectInput(
            "dia_doenca",
            "Dia de início dos sintomas",
            choices = c(
              "Não informado",
              "Dia 1 a 3",
              "Dia 4 a 7",
              "Após dia 7"
            ),
            selected = "Não informado"
          ),
          
          selectInput(
            "faixa",
            "Faixa etária",
            choices = c(
              "Não informado",
              "Adulto",
              "Criança",
              "Idoso",
              "Gestante"
            ),
            selected = "Não informado"
          ),
          
          checkboxInput(
            "comorbidade",
            "Há comorbidade ou condição especial?",
            FALSE
          )
        ),
        
        column(
          4,
          
          checkboxGroupInput(
            "sinais",
            "Sinais presentes",
            choices = c(
              "Dor abdominal intensa e contínua" = "dor",
              "Vômitos persistentes" = "vomitos",
              "Sangramento de mucosa" = "sangramento",
              "Lipotimia/hipotensão postural" = "lipotimia",
              "Letargia ou irritabilidade" = "alteracao",
              "Acúmulo de líquidos" = "liquidos"
            )
          )
        ),
        
        column(
          4,
          
          checkboxGroupInput(
            "sintomas",
            "Sintomas predominantes",
            choices = c(
              "Febre" = "febre",
              "Mialgia/cefaleia" = "mialgia",
              "Dor retro-orbitária" = "retro",
              "Artralgia intensa" = "artralgia",
              "Edema articular" = "edema",
              "Exantema" = "exantema"
            )
          ),
          
          div(
            style = "display: flex; gap: 10px; flex-wrap: wrap;",
            
            actionButton(
              "avaliar_btn",
              "Gerar orientação",
              class = "btn-primary"
            ),
            
            actionButton(
              "limpar_btn",
              "Limpar avaliação",
              class = "btn-secondary"
            )
          )
        )
      ),
      
      br(),
      
      div(
        class = "section-note",
        
        h3("Avaliação clínica complementar"),
        
        p(
          "Preencha quando houver informação disponível. ",
          "Esses campos apoiam a avaliação inicial e não substituem ",
          "exame físico, protocolo ou classificação clínica profissional."
        )
      ),
      
      fluidRow(
        
        column(
          3,
          
          numericInput(
            "pas",
            "Pressão arterial sistólica",
            value = NA,
            min = 0,
            max = 300
          ),
          
          numericInput(
            "pad",
            "Pressão arterial diastólica",
            value = NA,
            min = 0,
            max = 200
          )
        ),
        
        column(
          3,
          
          numericInput(
            "fc",
            "Frequência cardíaca",
            value = NA,
            min = 0,
            max = 300
          ),
          
          numericInput(
            "fr",
            "Frequência respiratória",
            value = NA,
            min = 0,
            max = 100
          )
        ),
        
        column(
          3,
          
          numericInput(
            "saturacao",
            "Saturação de oxigênio (%)",
            value = NA,
            min = 0,
            max = 100
          ),
          
          selectInput(
            "aceita_liquidos",
            "Aceitação de líquidos",
            choices = c(
              "Não informado",
              "Preservada",
              "Reduzida",
              "Não consegue ingerir"
            ),
            selected = "Não informado"
          )
        ),
        
        column(
          3,
          
          selectInput(
            "diurese",
            "Diurese",
            choices = c(
              "Não informado",
              "Preservada",
              "Reduzida",
              "Ausente"
            ),
            selected = "Não informado"
          ),
          
          checkboxInput(
            "gestacao_puerperio",
            "Gestação ou puerpério",
            value = FALSE
          )
        )
      ),
      
      fluidRow(
        
        column(
          12,
          
          checkboxGroupInput(
            "sinais_complementares",
            "Manifestações complementares presentes",
            choices = c(
              "Dor torácica ou palpitações" = "cardiaca",
              "Falta de ar ou desconforto respiratório" = "respiratoria",
              "Alteração da consciência, convulsão ou fraqueza" = "neurologica",
              "Dor articular incapacitante ou limitação importante" = "articular"
            )
          )
        )
      ),
      
      br(),
      
      uiOutput("resultado_avaliacao")
    )
  ),
  # -----------------------------------------------------------------------------
  # Aba de dengue x chikungunya
  # -----------------------------------------------------------------------------
  
  tabPanel(
    "Dengue x chikungunya",
    value = "comparar",
    
    div(
      class = "container-fluid",
      
      strong("Dengue x chikungunya"),
      
      div(
        class = "section-note",
        
        strong("Aviso de interpretação: "),
        
        "Nos primeiros dias, dengue e chikungunya podem apresentar manifestações semelhantes. ",
        "A diferenciação não deve ser feita por um único sintoma, e esta ferramenta não substitui ",
        "avaliação clínica, exames, classificação de risco ou reavaliação."
      ),
      
      h3("Comparação clínica"),
      
      tableOutput("comparacao"),
      
      h3("Como interpretar rapidamente"),
      
      div(
        class = "card-grid",
        
        div(
          class = "card",
          
          strong("Dengue"),
          
          h3(
            "Dar atenção especial aos sinais de alarme, sangramento, ",
            "acúmulo de líquidos, hipotensão, choque e comprometimento de órgãos."
          ),
          
          tags$ul(
            tags$li("Febre geralmente alta."),
            tags$li("Mialgia, cefaleia e dor retro-orbitária."),
            tags$li("Possibilidade de sangramento e extravasamento plasmático."),
            tags$li("Necessidade de reavaliação conforme a evolução da doença.")
          )
        ),
        
        div(
          class = "card",
          
          strong("Chikungunya"),
          
          h3(
            "Dar atenção especial à artralgia intensa, edema articular, ",
            "limitação funcional e manifestações extra-articulares."
          ),
          
          tags$ul(
            tags$li("Febre geralmente alta e de início súbito."),
            tags$li("Artralgia intensa e potencialmente incapacitante."),
            tags$li("Edema articular e rigidez podem estar presentes."),
            tags$li("Dor e manifestações articulares podem persistir por meses.")
          )
        ),
        
        div(
          class = "card",
          
          strong("Dengue X Chikungunya"),
          
          h3(
            "As duas doenças podem exigir acompanhamento, reavaliação ",
            "e articulação com o fluxo assistencial da rede."
          ),
          
          tags$ul(
            tags$li("A data de início dos sintomas é essencial."),
            tags$li("Sinais de gravidade devem ser avaliados imediatamente."),
            tags$li("Comorbidades e condições especiais modificam o risco."),
            tags$li("O diagnóstico não deve ser definido por um sintoma isolado.")
          )
        )
      ),
      
      h3("Quando reavaliar imediatamente"),
      
      div(
        class = "alert alert-danger",
        
        strong("Procure avaliação imediata conforme o fluxo vigente se houver:"),
        
        tags$ul(
          tags$li("Dor abdominal intensa e contínua."),
          tags$li("Vômitos persistentes ou incapacidade de ingerir líquidos."),
          tags$li("Sangramento de mucosa ou outros fenômenos hemorrágicos."),
          tags$li("Lipotimia, hipotensão postural ou sinais de choque."),
          tags$li("Falta de ar, cianose ou desconforto respiratório."),
          tags$li("Dor torácica, palpitações ou alteração do ritmo cardíaco."),
          tags$li("Alteração da consciência, convulsão, fraqueza ou parestesia."),
          tags$li("Redução ou ausência de diurese."),
          tags$li("Descompensação de doença preexistente.")
        ),
        
        p(
          "A presença de qualquer sinal de atenção exige reavaliação clínica ",
          "e seguimento do protocolo e do fluxo assistencial vigentes."
        )
      ),
      
      h3("Pontos práticos"),
      
      tags$ul(
        tags$li(
          "Artralgia intensa e edema articular favorecem chikungunya, ",
          "mas não excluem dengue."
        ),
        
        tags$li(
          "Mialgia, cefaleia e dor retro-orbitária são mais sugestivas de dengue, ",
          "mas não confirmam o diagnóstico."
        ),
        
        tags$li(
          "Plaquetopenia é mais característica da dengue, mas também pode ocorrer ",
          "na chikungunya."
        ),
        
        tags$li(
          "A data de início dos sintomas ajuda a interpretar a evolução clínica ",
          "e os exames indicados."
        ),
        
        tags$li(
          "A pessoa pode apresentar manifestações sobrepostas ou uma apresentação ",
          "atípica; por isso, a avaliação deve permanecer clínica e individualizada."
        )
      ),
      
      div(
        class = "section-note",
        
        strong("Importante: "),
        
        "Esta aba é uma ferramenta educativa e de apoio à consulta. ",
        "Não define diagnóstico, grupo de risco, necessidade de internação ",
        "ou destino assistencial."
      )
    )
  ),
  
  # -----------------------------------------------------------------------------
  # Aba exames
  # -----------------------------------------------------------------------------
  tabPanel(
    "Exames",
    value = "exames",
    
    div(
      class = "container-fluid",
      
      h2("Exames e coleta"),
      
      div(
        class = "alert alert-warning",
        
        strong("Atenção: "),
        
        "A indicação do exame, o momento da coleta e a interpretação do resultado ",
        "dependem da avaliação clínica, do dia de sintomas, do grupo de risco, ",
        "da gravidade e dos fluxos oficiais vigentes."
      ),
      
      h3("Apoio diagnóstico na assistência"),
      
      tags$details(
        class = "exame-collapse",
        
        tags$summary("Avaliação clínica"),
        
        div(
          class = "exame-content",
          
          p(
            "A conduta não deve ser atrasada pela espera de exame. ",
            "A classificação de risco, os sinais de alarme, os sinais vitais, ",
            "a hidratação e a reavaliação orientam o manejo inicial."
          ),
          
          tags$ul(
            tags$li("Avaliar o dia de início dos sintomas."),
            tags$li("Verificar sinais de alarme e sinais de gravidade."),
            tags$li("Considerar idade, gestação e comorbidades."),
            tags$li("Programar retorno e reavaliação conforme o caso.")
          )
        )
      ),
      
      tags$details(
        class = "exame-collapse",
        
        tags$summary("Exames inespecíficos"),
        
        div(
          class = "exame-content",
          
          p(
            "Quando indicados, os exames inespecíficos podem apoiar o ",
            "monitoramento clínico e a identificação de complicações."
          ),
          
          tags$ul(
            tags$li("Hemograma com contagem de plaquetas."),
            tags$li("Hematócrito, quando indicado."),
            tags$li("Transaminases."),
            tags$li("Ureia e creatinina."),
            tags$li("Outros exames conforme a suspeita clínica.")
          )
        )
      ),
      
      tags$details(
        class = "exame-collapse",
        
        tags$summary("Exames específicos"),
        
        div(
          class = "exame-content",
          
          p(
            "A escolha entre biologia molecular e sorologia deve considerar ",
            "o dia de sintomas, o agravo suspeito, o grupo do paciente e a ",
            "orientação vigente do LACEN-DF."
          ),
          
          tags$ul(
            tags$li("Dengue PCR."),
            tags$li("Dengue sorologia IgM."),
            tags$li("Chikungunya PCR."),
            tags$li("Chikungunya sorologia IgM ou IgG, quando indicada.")
          )
        )
      ),
      
      h3("Fluxo laboratorial de referência"),
      
      div(
        class = "section-note",
        
        tags$ol(
          tags$li(
            "Realizar avaliação clínica e definir a necessidade de coleta."
          ),
          
          tags$li(
            "Registrar corretamente a data de início dos sintomas."
          ),
          
          tags$li(
            "Cadastrar a requisição no sistema indicado pela rede, ",
            "incluindo o GAL-DF quando aplicável."
          ),
          
          tags$li(
            "Coletar, identificar e acondicionar a amostra conforme a ",
            "orientação específica do exame."
          ),
          
          tags$li(
            "Encaminhar a amostra com a documentação exigida."
          ),
          
          tags$li(
            "Acompanhar o resultado e realizar a interpretação em conjunto ",
            "com a avaliação clínica."
          )
        )
      ),
      
      h3("Tabela resumida de coleta"),
      
      div(
        class = "section-note",
        
        "A tabela é um resumo operacional. Antes da coleta e do envio, ",
        "confira a ficha do exame e a orientação vigente do LACEN-DF."
      ),
      
      DTOutput("tabela_exames"),
      
      h3("Detalhamento por exame"),
      
      tags$details(
        class = "exame-collapse",
        
        tags$summary("Dengue PCR"),
        
        div(
          class = "exame-content",
          
          p(
            strong("Método: "),
            "Pesquisa de ácido nucleico por RT-PCR em tempo real."
          ),
          
          p(
            strong("Cadastro: "),
            "Dengue PCR no TrakCare; código LabTrak: I214."
          ),
          
          p(
            strong("Pesquisa no GAL: "),
            "Pesquisa de Arbovírus (ZDC)."
          ),
          
          tags$ul(
            tags$li("Soro em tubo com gel separador — mínimo de 2 mL."),
            tags$li("Líquor em frasco estéril — mínimo de 2 mL."),
            tags$li("Coleta do 1º ao 5º dia de sintomas."),
            tags$li("Conservar entre 2 °C e 8 °C por até 48 horas."),
            tags$li("Enviar em até 48 horas."),
            tags$li("Prazo de liberação: até 12 dias úteis."),
            tags$li("Ficha SINAN.")
          ),
          a(
            class = "btn btn-primary",
            href = "https://www.lacendf.saude.df.gov.br/arbovirus-pesq-pcr/",
            target = "_blank",
            "Consultar página  Dengue PCR"
          )
        )
      ),
      
      tags$details(
        class = "exame-collapse",
        
        tags$summary("Dengue sorologia IgM"),
        
        div(
          class = "exame-content",
          
          p(
            strong("Método: "),
            "Pesquisa sorológica de anticorpos IgM contra dengue."
          ),
          
          p(
            "A indicação e o momento da coleta devem considerar o dia de ",
            "sintomas, a avaliação clínica e o fluxo vigente do LACEN-DF."
          ),
          
          a(
            class = "btn btn-primary",
            href = "https://www.lacendf.saude.df.gov.br/arbovirus-sorologia-p-dengue-elisa-3",
            target = "_blank",
            "Consultar página  Dengue sorologia IgM"
          )
        )
      ),
      
      tags$details(
        class = "exame-collapse",
        
        tags$summary("Chikungunya PCR"),
        
        div(
          class = "exame-content",
          
          p(
            strong("Método: "),
            "Pesquisa de ácido nucleico por RT-PCR em tempo real."
          ),
          
          p(
            strong("Cadastro: "),
            "Chikungunya PCR no TrakCare; código LabTrak: I215."
          ),
          
          p(
            strong("Pesquisa no GAL: "),
            "Pesquisa de Arbovírus (ZDC)."
          ),
          
          tags$ul(
            tags$li("Soro em tubo com gel separador — mínimo de 2 mL."),
            tags$li("Líquor em frasco estéril — mínimo de 2 mL."),
            tags$li("Coleta do 1º ao 5º dia de sintomas."),
            tags$li("Conservar entre 2 °C e 8 °C por até 48 horas."),
            tags$li("Enviar em até 48 horas."),
            tags$li("Prazo de liberação: até 12 dias úteis."),
            tags$li("Ficha SINAN devidamente preenchida.")
          ),
          
          a(
            class = "btn btn-primary",
            href = "https://www.lacendf.saude.df.gov.br/chikungunya-pcr/",
            target = "_blank",
            "Consultar página Chikungunya PCR"
          )
        )
      ),
      
      tags$details(
        class = "exame-collapse",
        
        tags$summary("Chikungunya sorologia IgM"),
        
        div(
          class = "exame-content",
          
          p(
            strong("Método: "),
            "MAC-ELISA para pesquisa de anticorpos IgM."
          ),
          
          p(
            strong("Cadastro: "),
            "Arbovírus, Chikungunya IgM, pesquisa de anticorpos; ",
            "código LabTrak: i149."
          ),
          
          tags$ul(
            tags$li("Soro em tubo com gel separador — mínimo de 2 mL."),
            tags$li("Líquor em frasco estéril — mínimo de 2 mL."),
            tags$li("Coleta a partir do 6º dia de sintomas."),
            tags$li("Conservar refrigerado entre 2 °C e 8 °C."),
            tags$li("Enviar em até 48 horas."),
            tags$li("Prazo de liberação: até 15 dias úteis."),
            tags$li("Ficha SINAN.")
          ),
          
          a(
            class = "btn btn-primary",
            href = "https://www.lacendf.saude.df.gov.br/arbovirus-chikungunya-igm/",
            target = "_blank",
            "Consultar página Chikungunya IgM"
          )
        )
      ),
      
      h3("Exames disponíveis no LACEN-DF"),
      
      tags$details(
        class = "exame-collapse",
        
        tags$summary("Consultar lista completa de exames"),
        
        div(
          class = "exame-content",
          
          tags$ul(
            tags$li("Dengue PCR."),
            tags$li("Dengue sorologia IgM."),
            tags$li("Chikungunya PCR."),
            tags$li("Chikungunya sorologia IgM."),
            tags$li("Chikungunya sorologia IgG.")
          ),
          
          a(
            class = "btn btn-primary",
            href = "https://lacendf.saude.df.gov.br/exames",
            target = "_blank",
            "Consultar exames no LACEN-DF"
          )
        )
      ),
      
      div(
        class = "section-note",
        
        strong("Importante: "),
        
        "Os fluxos, critérios de coleta, prazos, sistemas de cadastro, ",
        "documentos e condições de transporte devem ser conferidos nas ",
        "páginas oficiais antes do uso institucional."
      )
    )
  ),
  
  # -----------------------------------------------------------------------------
  # Aba Rede Norte
  # -----------------------------------------------------------------------------

tabPanel(
  "Rede Norte",
  value = "rede",
  
  div(
    class = "container-fluid",
    
    h2("Rede de Saúde Norte"),
    
    div(
      class = "alert alert-warning",
      role = "alert",
      
      strong("Uso profissional: "),
      
      "Esta aba apoia a identificação do ponto de atenção e do fluxo de referência. ",
      "A decisão assistencial deve considerar a avaliação clínica, os sinais de alarme, ",
      "a gravidade e os fluxos institucionais vigentes."
    ),
    
    h3("Fluxo rápido de referência"),
    
    div(
      class = "card-grid",
      
      div(
        class = "card",
        
        strong("Caso estável"),
        
        p(
          "Pessoa com suspeita de arbovirose, sem sinais de alarme ou gravidade, ",
          "pode ser acompanhada na UBS de referência, conforme avaliação clínica ",
          "e capacidade instalada da unidade."
        ),
        
        tags$ul(
          tags$li("Realizar acolhimento e avaliação clínica."),
          tags$li("Orientar hidratação e sinais de retorno."),
          tags$li("Programar reavaliação."),
          tags$li("Notificar conforme o fluxo vigente.")
        )
      ),
      
      div(
        class = "card",
        
        strong("Grupo de atenção diferenciada"),
        
        p(
          "Pessoas com gestação, idade avançada, infância, comorbidades ou ",
          "vulnerabilidade podem precisar de observação, exames e reavaliação ",
          "mais próxima, conforme o protocolo."
        ),
        
        tags$ul(
          tags$li("Confirmar condições especiais."),
          tags$li("Avaliar necessidade de exames."),
          tags$li("Verificar capacidade da unidade."),
          tags$li("Definir retorno e acompanhamento.")
        )
      ),
      
      div(
        class = "card",
        
        strong("Sinal de alarme ou gravidade"),
        
        p(
          "Na presença de sinal de alarme, instabilidade ou manifestação ",
          "extra-articular importante, acionar o serviço de urgência e seguir ",
          "o fluxo de referência e transporte definido pela rede."
        ),
        
        tags$ul(
          tags$li("Reavaliar imediatamente."),
          tags$li("Estabilizar conforme competência e protocolo."),
          tags$li("Comunicar a unidade de referência."),
          tags$li("Registrar o encaminhamento.")
        )
      )
    ),
    
    
    
    h3("Unidades da Região Norte"),
    
    div(
      class = "section-note",
      
      "A tabela abaixo apresenta as unidades cadastradas. ",
      "Preencha telefones, horários, endereços e capacidades assistenciais ",
      "somente com dados institucionais confirmados."
    ),
    
    # -----------------------------------------------------------------------------
    # Aba Rede Norte
    # -----------------------------------------------------------------------------
    
    DTOutput("tabela_rede"),
    h3("Como encaminhar"),
    
    tags$details(
      class = "exame-collapse",
      
      tags$summary("Antes do encaminhamento"),
      
      div(
        class = "exame-content",
        
        tags$ul(
          tags$li("Avaliar e registrar os sinais vitais."),
          tags$li("Verificar sinais de alarme e sinais de gravidade."),
          tags$li("Registrar o dia de início dos sintomas."),
          tags$li("Considerar idade, gestação, comorbidades e condições especiais."),
          tags$li("Registrar as condutas realizadas na unidade.")
        )
      )
    ),
    
    tags$details(
      class = "exame-collapse",
      
      tags$summary("Informações que devem acompanhar o paciente"),
      
      div(
        class = "exame-content",
        
        tags$ul(
          tags$li("Nome e idade."),
          tags$li("Data de início dos sintomas."),
          tags$li("Sinais e sintomas presentes."),
          tags$li("Sinais vitais registrados."),
          tags$li("Hidratação e diurese."),
          tags$li("Exames realizados ou solicitados."),
          tags$li("Motivo do encaminhamento."),
          tags$li("Unidade de destino.")
        )
      )
    ),
    
    tags$details(
      class = "exame-collapse",
      
      tags$summary("Após o atendimento na referência"),
      
      div(
        class = "exame-content",
        
        tags$ul(
          tags$li("Registrar o encaminhamento."),
          tags$li("Confirmar a unidade de destino."),
          tags$li("Orientar o retorno à UBS quando indicado."),
          tags$li("Acompanhar a contrarreferência."),
          tags$li("Programar a reavaliação.")
        )
      )
    ),
    

    div(
      class = "section-note",
      
      strong("Importante: "),
      
      "A rede pode mudar conforme o cenário epidemiológico, a capacidade ",
      "operacional e os fluxos pactuados. Confirme as informações antes de ",
      "orientar ou encaminhar o paciente."
    ),
    
    h3("Mapa das unidades"),
    
    div(
      class = "section-note",
      
      "O mapa apresenta as unidades cadastradas na planilha de coordenadas. ",
      "As informações devem ser conferidas antes do uso para encaminhamento."
    ),
    
    leafletOutput(
      "mapa_rede",
      height = "600px"
    ),
    h3("Encontrar unidade mais próxima"),
    
    div(
      class = "section-note",
      
      "Digite um endereço para localizar as unidades mais próximas. ",
      "O resultado será baseado na proximidade geográfica e não substitui ",
      "a confirmação do fluxo oficial de referência."
    ),
    
    textInput(
      "endereco_busca",
      "Endereço",
      placeholder = "Ex.: Quadra 1, Planaltina - DF"
    ),
    
    actionButton(
      "buscar_unidade",
      "Buscar unidade mais próxima",
      class = "btn-primary"
    ),
    
    br(),
    br(),
    
    uiOutput("resultado_unidade")
  )
),
  

# -----------------------------------------------------------------------------
# Aba Guia R Markdown
# -----------------------------------------------------------------------------
  tabPanel(
    "Referências",
    value = "guia",
    div(
      class = "container-fluid",
      h2("Guia documental"),
      includeMarkdown("rmd/guia_arboviroses.Rmd")
    )
  )
)
# -----------------------------------------------------------------------------
# Servidor
# -----------------------------------------------------------------------------
server <- function(input, output, session) {
  
  output$comparacao <- renderTable({
    data.frame(
      Caracteristica = c(
        "Febre",
        "Dor predominante",
        "Edema articular",
        "Plaquetopenia",
        "Risco principal"
      ),
      Dengue = c(
        "Geralmente alta",
        "Mialgia, cefaleia e dor retro-orbitária",
        "Menos comum",
        "Mais característica",
        "Sinais de alarme, choque, sangramento e lesão de órgãos"
      ),
      Chikungunya = c(
        "Geralmente alta",
        "Artralgia intensa e incapacitante",
        "Mais frequente",
        "Pode ocorrer",
        "Dor persistente, manifestações extra-articulares e formas graves"
      ),
      check.names = FALSE
    )
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
  # -----------------------------------------------------------------------------
  # Exames
  # -----------------------------------------------------------------------------
  
  output$tabela_exames <- renderDT(
    fluxos_lacen,
    options = list(
      pageLength = 5,
      scrollX = TRUE
    ),
    rownames = FALSE,
    filter = "top"
  )
  # -----------------------------------------------------------------------------
  # Rede Norte
  # -----------------------------------------------------------------------------
  output$tabela_rede <- renderDT(
    rede_norte,
    options = list(
      pageLength = 10,
      scrollX = TRUE
    ),
    rownames = FALSE,
    filter = "top"
  )
  # -----------------------------------------------------------------------------
  # Mapa Rede Norte
  # -----------------------------------------------------------------------------
  output$mapa_rede <- renderLeaflet({
    
    cores <- ifelse(
      grepl("UPA", coordenadas_rede$Unidade, ignore.case = TRUE),
      "#d9534f",
      ifelse(
        grepl("HRPL|HRS|HOSPITAL", coordenadas_rede$Unidade, ignore.case = TRUE),
        "#102f63",
        "#2e8b57"
      )
    )
    
    popups <- paste0(
      "<strong>", coordenadas_rede$Unidade, "</strong>",
      "<br>Território: ", coordenadas_rede$Territorio,
      "<br>Latitude: ", coordenadas_rede$Latitude,
      "<br>Longitude: ", coordenadas_rede$Longitude
    )
    latitude_minima <- min(
      coordenadas_rede$Latitude,
      na.rm = TRUE
    )
    
    latitude_maxima <- max(
      coordenadas_rede$Latitude,
      na.rm = TRUE
    )
    
    longitude_minima <- min(
      coordenadas_rede$Longitude,
      na.rm = TRUE
    )
    
    longitude_maxima <- max(
      coordenadas_rede$Longitude,
      na.rm = TRUE
    )
    
    margem_latitude <- 0.03
    margem_longitude <- 0.03
    leaflet(
      coordenadas_rede,
      options = leafletOptions(
        minZoom = 10,
        maxZoom = 18
      )
    ) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        radius = 7,
        stroke = TRUE,
        weight = 2,
        color = cores,
        fillColor = cores,
        fillOpacity = 0.85,
        popup = popups
      ) %>%
      fitBounds(
        lng1 = longitude_minima,
        lat1 = latitude_minima,
        lng2 = longitude_maxima,
        lat2 = latitude_maxima
      ) %>%
      setMaxBounds(
        lng1 = longitude_minima - margem_longitude,
        lat1 = latitude_minima - margem_latitude,
        lng2 = longitude_maxima + margem_longitude,
        lat2 = latitude_maxima + margem_latitude
      ) %>%
      addLegend(
        position = "bottomright",
        colors = c(
          "#2e8b57",
          "#d9534f",
          "#102f63"
        ),
        labels = c(
          "UBS",
          "UPA",
          "Hospital"
        ),
        title = "Tipo de unidade",
        opacity = 1
      )
  })
  output$resultado_unidade <- renderUI({
    
    req(input$buscar_unidade)
    
    endereco <- trimws(input$endereco_busca)
    
    if (endereco == "") {
      return(
        div(
          class = "alert alert-warning",
          "Digite um endereço antes de buscar."
        )
      )
    }
    
    resultado_endereco <- tryCatch(
      {
        geo_lite(
          address = paste(endereco, "Distrito Federal, Brasil"),
          limit = 1
        )
      },
      error = function(e) {
        NULL
      }
    )
    
    if (
      is.null(resultado_endereco) ||
      nrow(resultado_endereco) == 0
    ) {
      return(
        div(
          class = "alert alert-warning",
          "Não foi possível localizar esse endereço. ",
          "Tente informar rua, número, cidade e UF."
        )
      )
    }
    
    latitude_usuario <- resultado_endereco$lat[1]
    longitude_usuario <- resultado_endereco$lon[1]
    mapa <- leafletProxy("mapa_rede")
    
    mapa %>%
      clearGroup("endereco_usuario") %>%
      addAwesomeMarkers(
        lng = longitude_usuario,
        lat = latitude_usuario,
        group = "endereco_usuario",
        icon = awesomeIcons(
          icon = "home",
          library = "glyphicon",
          markerColor = "blue"
        ),
        popup = paste0(
          "<strong>Endereço pesquisado</strong><br>",
          endereco
        )
      )
    coordenadas_rede$Distancia_km <- geosphere::distHaversine(
      cbind(
        coordenadas_rede$Longitude,
        coordenadas_rede$Latitude
      ),
      c(
        longitude_usuario,
        latitude_usuario
      )
    ) / 1000
    
    coordenadas_rede$Tipo <- ifelse(
      grepl(
        "UPA",
        coordenadas_rede$Unidade,
        ignore.case = TRUE
      ),
      "UPA",
      ifelse(
        grepl(
          "HRPL|HRS|HOSPITAL",
          coordenadas_rede$Unidade,
          ignore.case = TRUE
        ),
        "Hospital",
        "UBS"
      )
    )
    
    unidades_ordenadas <- coordenadas_rede[
      order(coordenadas_rede$Distancia_km),
    ]
    
    ubs <- unidades_ordenadas[
      unidades_ordenadas$Tipo == "UBS",
      ,
      drop = FALSE
    ]
    
    upa <- unidades_ordenadas[
      unidades_ordenadas$Tipo == "UPA",
      ,
      drop = FALSE
    ]
    
    hospital <- unidades_ordenadas[
      unidades_ordenadas$Tipo == "Hospital",
      ,
      drop = FALSE
    ]
    
    resultado_unidade <- function(
    dados,
    titulo,
    cor
    ) {
      
      if (nrow(dados) == 0) {
        return(
          div(
            class = "card",
            h4(titulo),
            p("Nenhuma unidade cadastrada.")
          )
        )
      }
      
      div(
        class = "card",
        style = paste0("border-left: 5px solid ", cor, ";"),
        
        h4(titulo),
        
        p(
          strong(dados$Unidade[1])
        ),
        
        p(
          "Território: ",
          dados$Territorio[1]
        ),
        
        p(
          "Distância aproximada: ",
          format(
            round(dados$Distancia_km[1], 1),
            nsmall = 1
          ),
          " km"
        )
      )
    }
    
    div(
      class = "card-grid",
      
      div(
        class = "section-note",
        
        strong("Endereço localizado: "),
        endereco,
        br(),
        "Latitude: ",
        round(latitude_usuario, 6),
        " | Longitude: ",
        round(longitude_usuario, 6)
      ),
      
      resultado_unidade(
        ubs,
        "UBS mais próxima",
        "#2e8b57"
      ),
      
      resultado_unidade(
        upa,
        "UPA mais próxima",
        "#d9534f"
      ),
      
      resultado_unidade(
        hospital,
        "Hospital mais próximo",
        "#102f63"
      ),
      
      div(
        class = "section-note",
        
        strong("Atenção: "),
        
        "O resultado representa proximidade geográfica. ",
        "Confirme o fluxo oficial de referência antes de encaminhar."
      )
    )
  })
  # -----------------------------------------------------------------------------
  # Avaliação do paciente
  # -----------------------------------------------------------------------------
  output$resultado_avaliacao <- renderUI({
    req(input$avaliar_btn)
    
    sinais <- input$sinais %||% character(0)
    sintomas <- input$sintomas %||% character(0)
    sinais_complementares <- input$sinais_complementares %||% character(0)
    
    sinais_complementares_nomes <- c(
      cardiaca = "Dor torácica ou palpitações",
      respiratoria = "Falta de ar ou desconforto respiratório",
      neurologica = "Alteração da consciência, convulsão ou fraqueza",
      articular = "Dor articular incapacitante ou limitação importante"
    )
    
    valor_informado <- function(valor) {
      if (is.null(valor) || length(valor) == 0 || is.na(valor)) {
        "Não informado"
      } else {
        as.character(valor)
      }
    }
    
    resultado_complementar <- div(
      br(),
      
      strong("Avaliação clínica complementar:"),
      
      p(
        "Pressão arterial: ",
        valor_informado(input$pas),
        " / ",
        valor_informado(input$pad),
        " mmHg"
      ),
      
      p(
        "Frequência cardíaca: ",
        valor_informado(input$fc),
        " bpm | Frequência respiratória: ",
        valor_informado(input$fr),
        " irpm"
      ),
      
      p(
        "Saturação de oxigênio: ",
        valor_informado(input$saturacao),
        "%"
      ),
      
      p(
        "Aceitação de líquidos: ",
        valor_informado(input$aceita_liquidos),
        " | Diurese: ",
        valor_informado(input$diurese)
      ),
      
      p(
        "Gestação ou puerpério: ",
        ifelse(
          isTRUE(input$gestacao_puerperio),
          "Sim",
          "Não"
        )
      ),
      
      if (length(sinais_complementares) > 0) {
        p(
          strong("Manifestações complementares: "),
          paste(
            unname(
              sinais_complementares_nomes[sinais_complementares]
            ),
            collapse = ", "
          )
        )
      } else {
        p(
          strong("Manifestações complementares: "),
          "Nenhuma selecionada"
        )
      }
    )
    
    sintomas_nomes <- c(
      febre = "Febre",
      mialgia = "Mialgia/cefaleia",
      retro = "Dor retro-orbitária",
      artralgia = "Artralgia intensa",
      edema = "Edema articular",
      exantema = "Exantema"
    )
    
    sinais_nomes <- c(
      dor = "Dor abdominal intensa e contínua",
      vomitos = "Vômitos persistentes",
      sangramento = "Sangramento de mucosa",
      lipotimia = "Lipotimia/hipotensão postural",
      alteracao = "Letargia ou irritabilidade",
      liquidos = "Acúmulo de líquidos"
    )
    
    resultado_sintomas <- if (length(sintomas) > 0) {
      p(
        strong("Sintomas predominantes: "),
        paste(
          unname(sintomas_nomes[sintomas]),
          collapse = ", "
        )
      )
    } else {
      p(
        strong("Sintomas predominantes: "),
        "Nenhum selecionado"
      )
    }
    # -------------------------------------------------------------------------
    # Alertas clínicos para revisão
    # -------------------------------------------------------------------------
    
    alertas_clinicos <- character(0)
    
    pressao_pulso <- if (
      !is.null(input$pas) &&
      !is.null(input$pad) &&
      length(input$pas) > 0 &&
      length(input$pad) > 0 &&
      !is.na(input$pas) &&
      !is.na(input$pad)
    ) {
      input$pas - input$pad
    } else {
      NA_real_
    }
    
    if (!is.na(pressao_pulso) && pressao_pulso <= 20) {
      alertas_clinicos <- c(
        alertas_clinicos,
        paste0(
          "Pressão de pulso reduzida (",
          pressao_pulso,
          " mmHg)."
        )
      )
    }
    
    if ("respiratoria" %in% sinais_complementares) {
      alertas_clinicos <- c(
        alertas_clinicos,
        "Falta de ar ou desconforto respiratório selecionado."
      )
    }
    
    if ("cardiaca" %in% sinais_complementares) {
      alertas_clinicos <- c(
        alertas_clinicos,
        "Dor torácica ou palpitações selecionadas."
      )
    }
    
    if ("neurologica" %in% sinais_complementares) {
      alertas_clinicos <- c(
        alertas_clinicos,
        "Manifestação neurológica selecionada."
      )
    }
    
    if (input$diurese %in% c("Reduzida", "Ausente")) {
      alertas_clinicos <- c(
        alertas_clinicos,
        paste0(
          "Diurese ",
          tolower(input$diurese),
          " informada."
        )
      )
    }
    
    resultado_alertas <- if (length(alertas_clinicos) > 0) {
      div(
        class = "alert alert-warning",
        
        h4("Atenção: dado(s) clínico(s) para revisão"),
        
        tags$ul(
          lapply(
            alertas_clinicos,
            tags$li
          )
        ),
        
        p(
          "Reavalie o paciente, confirme os dados, realize exame clínico ",
          "completo e siga o protocolo e o fluxo assistencial vigentes."
        ),
        
        p(
          strong("A ferramenta não define diagnóstico, grupo de risco, ",
                 "necessidade de internação ou destino assistencial.")
        )
      )
    } else {
      NULL
    }
    if (length(sinais) > 0) {
      
      div(
        class = "alert alert-danger",
        
        h4("Atenção: há sinal(is) de alarme selecionado(s)."),
        
        p(
          "Reavalie imediatamente e siga o protocolo e o fluxo ",
          "de encaminhamento vigentes. Esta ferramenta não define ",
          "diagnóstico ou destino assistencial."
        ),
        
        p(
          strong("Sinais selecionados: "),
          paste(
            unname(sinais_nomes[sinais]),
            collapse = ", "
          )
        ),
        
        p(
          strong("Perfil informado: "),
          input$dia_doenca,
          " | ",
          input$faixa,
          " | Comorbidade ou condição especial: ",
          ifelse(
            isTRUE(input$comorbidade),
            "Sim",
            "Não"
          )
        ),
        
        resultado_sintomas,
        resultado_complementar,
        resultado_alertas
      )
      
    } else {
      
      div(
        class = "section-note",
        
        h4("Sem sinal de alarme selecionado"),
        
        p(
          "Continue a avaliação clínica, considerando o dia de doença, ",
          "condições especiais, hidratação, orientação de retorno, ",
          "exames indicados e notificação conforme fluxo vigente."
        ),
        
        p(
          strong("Perfil informado: "),
          input$dia_doenca,
          " | ",
          input$faixa,
          " | Comorbidade ou condição especial: ",
          if (isTRUE(input$comorbidade)) "Sim" else "Não"
        ),
        
        resultado_sintomas,
        resultado_complementar,
        resultado_alertas
      )
    }
  })
  
  
  # ---------------------------------------------------------------------------
  # Botão Limpar avaliação
  # ---------------------------------------------------------------------------
  observeEvent(input$limpar_btn, {
    
    updateSelectInput(
      session,
      "dia_doenca",
      selected = "Não informado"
    )
    
    updateSelectInput(
      session,
      "faixa",
      selected = "Não informado"
    )
    
    updateCheckboxInput(
      session,
      "comorbidade",
      value = FALSE
    )
    
    updateCheckboxGroupInput(
      session,
      "sinais",
      selected = character(0)
    )
    
    updateCheckboxGroupInput(
      session,
      "sintomas",
      selected = character(0)
    )
    updateNumericInput(
      session,
      "pas",
      value = NA
    )
    
    updateNumericInput(
      session,
      "pad",
      value = NA
    )
    
    updateNumericInput(
      session,
      "fc",
      value = NA
    )
    
    updateNumericInput(
      session,
      "fr",
      value = NA
    )
    
    updateNumericInput(
      session,
      "saturacao",
      value = NA
    )
    
    updateSelectInput(
      session,
      "aceita_liquidos",
      selected = "Não informado"
    )
    
    updateSelectInput(
      session,
      "diurese",
      selected = "Não informado"
    )
    
    updateCheckboxInput(
      session,
      "gestacao_puerperio",
      value = FALSE
    )
    
    updateCheckboxGroupInput(
      session,
      "sinais_complementares",
      selected = character(0)
    )
  })
}


`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}


shinyApp(ui, server)
