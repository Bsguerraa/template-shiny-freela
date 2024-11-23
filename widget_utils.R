# options = pickerOptions(container = "body",
#                         title = "SELECIONE",
#                         actionsBox = TRUE,
#                         liveSearch = TRUE,
#                         selectAllText = "SELECIONAR TODOS",
#                         deselectAllText = "LIMPAR TODOS",
#                         noneSelectedText= "SELECIONE",
#                         noneResultsText = "SEM CORRESPONDÊNCIAS",
#                         selectedTextFormat = "count > 3",
#                         countSelectedText = "{0} ITENS SELECIONADOS"),
# width = "100%"
# ),
# column(1),


pickerInput(
        inputId = "month",
        label = "Select a month",
        choices = month.name,
        multiple = TRUE,
        options = pickerOptions(container = "body",
                                title = "SELECIONE",
                                actionsBox = TRUE,
                                liveSearch = TRUE,
                                selectAllText = "SELECIONAR TODOS",
                                deselectAllText = "LIMPAR TODOS",
                                noneSelectedText= "SELECIONE",
                                noneResultsText = "SEM CORRESPONDÊNCIAS",
                                selectedTextFormat = "count > 3",
                                countSelectedText = "{0} ITENS SELECIONADOS")
)


shinyWidgets::shinyWidgetsGallery()
