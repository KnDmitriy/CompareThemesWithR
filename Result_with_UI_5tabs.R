InstallOrLoadPack <- function(packs){
  create.pkg <- packs[!(packs %in% installed.packages()[, "Package"])]
  if (length(create.pkg))
    install.packages(create.pkg, dependencies = TRUE)
  sapply(packs, library, character.only = TRUE)
}
packages <- c("ggrepel", "ggplot2",  "data.table", "tm", "wordcloud2", "tidytext", 
              "dplyr", 'tidyverse', 'readxl', 'udpipe', 'writexl', 'openxlsx', 'rlang', 
              'lsa', 'shiny')
 
InstallOrLoadPack(packages)

Wordcloud2a <- function (data, size = 1, minSize = 0, gridSize = 0, fontFamily = "Segoe UI", 
                         fontWeight = "bold", color = "random-dark", backgroundColor = "white", 
                         minRotation = -pi/4, maxRotation = pi/4, shuffle = TRUE, 
                         rotateRatio = 0.4, shape = "circle", ellipticity = 0.65, 
                         widgetsize = NULL, figPath = NULL, hoverFunction = NULL) 
{
  if ("table" %in% class(data)) {
    dataOut = data.frame(name = names(data), freq = as.vector(data))
  }
  else {
    data = as.data.frame(data)
    dataOut = data[, 1:2]
    names(dataOut) = c("name", "freq")
  }
  if (!is.null(figPath)) {
    if (!file.exists(figPath)) {
      stop("cannot find fig in the figPath")
    }
    spPath = strsplit(figPath, "\\.")[[1]]
    len = length(spPath)
    figClass = spPath[len]
    if (!figClass %in% c("jpeg", "jpg", "png", "bmp", "gif")) {
      stop("file should be a jpeg, jpg, png, bmp or gif file!")
    }
    base64 = base64enc::base64encode(figPath)
    base64 = paste0("data:image/", figClass, ";base64,", 
                    base64)
  }
  else {
    base64 = NULL
  }
  weightFactor = size * 180/max(dataOut$freq)
  settings <- list(word = dataOut$name, freq = dataOut$freq, 
     fontFamily = fontFamily, fontWeight = fontWeight, 
     color = color, minSize = minSize, weightFactor = weightFactor, 
     backgroundColor = backgroundColor, 
     gridSize = gridSize, minRotation = minRotation, 
     maxRotation = maxRotation, shuffle = shuffle, 
     rotateRatio = rotateRatio, shape = shape, 
     ellipticity = ellipticity, figBase64 = base64, 
     hover = htmlwidgets::JS(hoverFunction))
  chart = htmlwidgets::createWidget("wordcloud2", settings, 
    width = widgetsize[1], height = widgetsize[2], 
    sizingPolicy = htmlwidgets::sizingPolicy(viewer.padding = 0, 
                           browser.padding = 0, browser.fill = TRUE))
  return(chart)
}




GetListOfStopwords <- function() {
  
  female_names_rus <- read.csv("female_names_rus.txt", header=FALSE)
  male_names_rus <- read.csv("male_names_rus.txt", header=FALSE)
  male_surnames_rus <- read.csv("male_surnames_rus.txt", header=FALSE)
  stop_words_expanded <- c('и', 'й', 'г', 'единаяроссия', 'единый', 'время', 
     'территория', 'димитровграда', 'димитровград', 
     'чебоксар', 'ядринский', 'житель', 'компания', 
     'министр', 'дом', 'общественный',
     'программа', 'мероприятие', 'условие', 'ситуация', 
     'гражданин', 'группа', 'организация',
     'система', 'оао', 'ооо', 'пао', 'зао', 'центр', 
     'руководитель', 'конкурс', 'решить', 'говорить',
     'состав', 'уровень', 'адрес', 'дело', 'заместитель', 
     'просто', 'данный', 'сайт', 'позолять',
     'директор', 'полный', 'час', 'неделя', 'рука', 'ядрин', 
     'погода', 'выбрать',
     'средство', 'принять', 'чебоксарский', 'пункт', 
     'получать', 'второй', 'количество',
     'найти', 'pgrunews', 'хороший', 'занятие', 'ныжный', 
     'номер', 'отлично', 'лс',
     'состояние', 'цена', 'вид', 'руба', 'зеркадать', 'ть', 
     'торг', 'нужный', 'муниципальный',
     'округ', 'ип', 'ссылка', 'задний', 'друг', 'замить', 
     'бесплатный', 
     'участник', 'слово', 'прогноз', 'удобный', 'тд', 
     'летний', 'лето',
     'астраханский', 'астрахань', 'астраханскаяобласть', 
     'федеральный', 'государственный',
     'министерство', 'команда', 'асраханец', 'еррб', 
     'россии', 'ербашкортостан', 'мера',
     'гтркбашкортостан', 'необходимый', 'возможность', 
     'предварительный', 'отделение',
     'объект', 'погодавбашкирия', 'gtrkrb', 'проходить', 
     'правительство', 'премьерминистр',
     'правительстворб', 'период', 'пресечить', 'сотрудник', 
     'гтрквяток', 'еркир', 'мс',
     'мс', 'ночь', 'мй', 'обещать', 'ожидаться', 'c',
     
     'правительствомарийэть', 'йошкарола', 'цур', 'цура', 
     'врио', 'федерация', 'исполнять',
     'врио', 'временно', 'заседание', 'эфир', 'цель', 
     'направить', 'внимание',
     'встреча', 'зайцин', 'сфера', 'национальный', 'улица', 
     'провдоить', 
     'нижегородскийрайон', 'нижнийновгород', 'пространство',
     'урка',
     'нижегородец', 'администрация', 'департамент', 
     'горький', 'шалабай', 'номинация',
     'градус', 'ребенок', 'ребёнок', 'ый', 'находится', 
     'находиться', 'сильный',
     'перй', 'ул', 'прогнозируть', 'красноярский', 
     'самар', 'толльятъ', 'тольять',
     'толльять','тольятть', 'ярин', 'иметься', 'самарь', 
     'требоваться', 'зп', 'сутка','двое',
     'магазин', 'саратов', 'сарат', 'составить', 'течение', 
     'случай', 'общий', 'энгельсский',
     'саратовец', 'саратовый', 'балашовский', 'начало', 
     'областной', 'составлять',
     'учреждение', 'совет', 'комитет', 'казанский', 
     'средний', 'азнакаевский',
     'удмуртский', 'ижевска', 'мояудмуртия', 'ува', 
     'увинский', 'ижевсок', 'ижевск', 'астраханец',
     'кировчанин',
     
     'образовательный', 'этап', 'астраханец', 'проведение', 
     'спортивный', 'дорожный',
     'градус', 'федеральный', 'цуринформировать', 'заявка', 
     'спортсмен', 'победитель', 'всероссийский',
     'результат', 'учебный', 'современный', 'цурмарийэл', 
     'зарегистрировать', 'совещание',
     'деятельность', 'акция', 'проведение', 'мероприятие', 
     'главный', 'несколько', 'любой',
     'площадка', 'комиссия', 'зона', 'помощь', 'специалист',
     'окружающий', 'профессиональный',
     'сельский', 'ремний', 'предложение', 'планироваться',
     'местный', 'онапомни', 'небольшой',
     'планироваться', 'председатель', 'совещание', 
     'обращение', 'республиканский', 'дополнительный',
     'индивидуальный', 'работник', 'режим', 'решение', 
     'управление', 'марийский', 'позволять',
     'деятельность', 'текущий', 'обсудить', 'поблагодарить', 
     'посетить', 'направление', 'тема',
     'орган', 'установить', 'участок', 'важноenn', 'пятница',
     'свердлово', 'транспортный',
     'программа', 'ерпенза', 'коронавирусный', 'необходимый',
     'возраст', 'возможность',
     'ситуация', 'средство', 'условие', 'уровень', 
     'количество', 'неделя', 'участник',
     'состояние', 'данный', 'позволять', 'адрес', 
     'охотничий', 'помогать', 'данный',
     'проводиться', 'здоровый', 'образ', 'активность', 
     'принять', 'httpspgrunews', 'профессия',
     'районный', 'действовать', 'заниматься', 'вместе', 
     'телефон', 'телефона', 'пробегнуть',
     'отличный', 'хотеть', 'ггра', 'сарансок', 'создание', 
     'смотреть', 'легкой',
     'оренбургское', 'профилактический', 'обстоятельство', 
     'поступить', 'произойти', 'сообщение',
     'следовать', 'пермь', 'застать', 'желать', 'линия', 
     'пусть', 'ближайший', 'рядом',
     'махонин', 'требование', 'внимательный', 'появиться', 
     'установить', 'режим', 'активный',
     'несколько', 'категория', 'позволять', 'смен', 
     'сменный', 'энгельс', 'срок', 'балаковский',
     'начать', 'план', 'прошлый', 'мр', 'услуга', 'лицо', 
     'специальный', 'заявка', 'httpskazanfirstrunews',
     "требовать", "править", "информация", 'обращение', 
     'править', 'население', 'подготовка', 'ремонт', 
     'план', 'комплекс', 'маршрут', 'снег', 'гражданин', 
     'общественный', 'продукт', 'товар', 'дом', 
     'мероприятие', 'система', 'сохранение', 'проблема', 
     'труд',  'деньги',  'мир', 'проверка', 
     'обратиться', 'возбудить', 'факт',  
     'избегать', 'соблюдение', 'явление', 'здание', 
     'данные', 'правило', 'график', 'источник', 'продукция',
     'дорога', 'транспорт', 'татарский', 'азнакаево', 
     'следующий', 'татарстанный', 'парламент', 
     'httpsvkcomappformidformid', 'выходный', 'ахмадинур',
     "управлять", "объем", "происшествие",
     "использование", "обеспечение", "использование", 
     "зауралье", "оперативный", "представитель",
     "голосоаваний", "секретарь", "рустема", "документ", 
     "считать", "минувший", "столица", "международный",
     "дождь", "м", "кубок", "фонд", "поздравляе", "мужчина", "женщина", "проводить", "пробный",
     "узнать", "материал", "статья", "отдел", "денежный", "полицейский", "сторона", "допустить", 
     "вещество", "соблюдать", "вызов", "мобильный", "знакомый", "родной", "прохождение",
     "плата", "выплата", "качество", "официальный", "комфортный", "оборудование", "обязанность",
     "добавить", "предоставление", "размер", "поставить", "касаться", "подготовить", "известный",
     "госсоветерт", "госсоветерт", "ч", "отношение", "завтра",
     
     'ульяновскаяобласть', 'ульяновск', 'ульяновский', 'саранск', 'саранска', 'мордовие', 'рм', 'рма', 'мордовия', 
     'мордовский', 'заец', 'idюрий', 'главамарийэл', 'марий', 'эл', 'марийэл', 'эть', 'васил', 'чурин', 'кировский', 
     'кировскаяобласть', 'вятский', 'мельниченко', 'месяц', 'оренбургнуть', 'объясняемрф', 'провести', 'инвестор', 
     'вести', 'реализация', 'башкортостанный', 'радий', 'подписать', 'проект', 'пермский', 'пермскийкрай', 'край', 
     'прикамья', 'краевой', 'задача', 'важно', 'оренбуржец', 'оренбург', 'новость', 'подчеркнуть', 'оренбуржье', 
     'оренбургский', 'оренбургскаяобласть', 'поддержка', 'часть', 'км', 'валерийрадаеть', 'олегнуть', 'должен', 
     'около', 'рассказать', 'глава', 'губернатор', 'развитие', 'январь', 'февраль', 'март', 'апрель', 'май', 
     'июнь', 'июль', 'август', 'сентябрь', 'октябрь', 'ноябрь', 'декабрь', 'город', 'ecom', 'казань', 'подробность', 
     'подробный', 'радия', 'процент', 'уф', 'часть', 'вопрос', 'делать', 'сделать', 'благодаря', 'участие', 'пройти', 
     'идти', 'создать', 'создавать', 'дать',  'рамка', 'место', 'получить', 'удмуртия', 'радай', 'юлие', 
     'пензенский', 'пенза', 'пензенскаяобласть', 'новый', 'лучший', 'самый', 'работа', 'рабочий', 'работать', 
     'региональный', 'нижегородскаяобладать', 'clubнижегородский', 'нижегородскаяобласть', 'нижегородский', 
     'нижний', 'новгород', 'чувашия', 'чувашие', 'обть', "бaшҡортостать", "бaшҡортостан", 'командахабиров', 'рб', 
     'миллиард', 'башкирия', 'башкортостан', 'башкортостана', 'мый', 'аный', 'мухаметшина', 'мухаметшин', 'реть', 
     'рф', 'день', 'отметить', 'число', 'миллион', 'ход', 'президент','страна', 'тысяча', 'рубль', 'доллар', 'район', 
     'итог', 'татарстан', 'татарстать', 'российский', 'ма', 'область', 'республика', 'саратовский', 'татарстан', 
      'татарстана', 'самарский','экономический', 'экономика', 'регион', 'год', "миннихан", "рт", "россия", "рустам",
      "руст", 'россия', 'конкурентоспособность', 'инновация', 'инвестиция', 'инвестиционный', 'рустамминнихан', 
      'дмитрий', 'азаров', 'саратовскаяобласть', 'саратовская', 'самарскаяобласть', 'азар', 'стать', 'rn«', 'твой',
      'сих', 'ком', 'свой',
     'слишком', 'нами', 'всему', 'будь', 'саму', 'чаще', 'ваше', 'наш', 'затем', 'еще', 'наши', 'ту', 'каждый',
     'мочь', 'весь', 'этим', 'наша', 'своих', 'оба', 'который', 'зато', 'те', 'вся', 'ваш', 'такая', 'теми', 'ею', 'нередко',
     'также', 'чему', 'собой', 'нем', 'вами', 'ими', 'откуда', 'такие', 'тому', 'та', 'очень', 'нему',  'д',
     'алло', 'оно', 'кому', 'тобой', 'таки', 'мой', 'нею', 'ваши', 'ваша', 'кем', 'мои',
     'однако', 'сразу', 'свое', 'ними', 'всё', 'неё', 'тех', 'хотя', 'всем', 'тобою', 'тебе', 'одной', 'другие',
     'буду', 'моё', 'своей', 'такое', 'всею', 'будут', 'своего', 'кого', 'свои', 'мог', 'нам', 'особенно', 'её',
     'наше', 'кроме', 'вообще', 'вон', 'мною', 'никто', 'это', 'изза', 'именно', 'поэтому', 'будьт', 'являться', 
     'чувашский', 'тыса', 'смочь', 'ваший', 'гльба', 'ать', 'уть', 'ивать', 'ольги', 'пенз', 'ер', 'иметь', 'олегнуть', 
     'сг', 'например', 'сообщить', 'сообщать', 'среди', 'нть', 'пер', 'зспермь', 'края', 'ради', 'назвать', 'важный',
     'ик', 'ульяновсок', 'ульяновска', 'russia', 'reg', 'видео', 'russianpolice', 'mvd', 'police', 'ульяновскаяобласть', 
      'ульяновскаобласть', 'русский', 'личный',
     
     'игтисамов', 'ирек', 'сагит', 'илшат', 'тажитдин', 
     'ильшат', 'фазрахман', 'моусошгкаменкапензенскаяобласть',
     'руб.', '-й', 'рубла', 'лир', 'рисок', 'уловек', 'месяц.',
     'рубла.',
     
     'ао', 'гбуз', 'амокб', 'оикб', '⃣⃣', '⃣', 'егора', 'угар',
     'омский', 'омск', 'бурк', 'омскаяобладать', 'омич',
     'омсок', 'омска', 'омскаяобласть', 'виталие', 'хоценко',
     'idвиталие', 'таврический', 'оу', 'марат', 'зяббар',
     'татарстанец', 'россиянин', 'татарстанский',
     'кафй', 'инь', 'янь', 'казанить', 'историитатарстана',
     'вказанить', 'онашо', 'cheeklaweek', 'bubblgum', 'dj',
     'онлайн', 'диляр', 'вагапов', 'фатхетдин', 'фирдус', 
     'тямай', 'артура', 'шамгун', 'ришат', 'тухватуллина',
     'имотреть', 'рубрика', 'цитатадня', 'подтврждёть', 'осыпный',
     'тасср', 'летие', 'леттасср', 'поставьть', 'потатарстана',
     'многий', 'радиф', 'кашап', 'ильяс', 'гафар', 'леттассругой',
     'посвящеть', 'мурак', 'раис', 'илий', 'шорин', 'подтверждёть', 
     'удерживать', 'квадратный', 'метр', 'температура',
     'первыетаврический', 'нашее', 'любима', 'историитатарстанный',
     'ахтубинский', 'наримановский', 'умерло', 'погодаомск',
     'рдд', 'хештег', 'особый', 'рддй', 'idвиталий', 'буть',
     'мбоу', 'сош', 'айдадомой', 'выступить'

                           
  )
  
  
  extra_stop_words <- stop_words_expanded
  stopwords_combined_list <- c(stopwords("russian"), extra_stop_words,
                               tolower(male_names_rus$V1),
                               tolower(male_surnames_rus$V1),
                               tolower(female_names_rus$V1))
  return(stopwords_combined_list)
}

stopwords_combined_list <- GetListOfStopwords()
stopwords_combined_str <- paste(stopwords_combined_list, collapse = "|")
basic_punctuation_marks_list <- c('.', ',', ';', ':', '!', '?', '-', '"', '(',
                                  ')', '«', '»')

label_radio_buttons <- "Метод излечения ключевых слов:"
label_action_output_results <- "Вывод результатов"
label_choose_file <- "Выбор файла"
label_input_file_button <- "Открыть..."
label_input_placeholder <- "Файл не выбран"
label_calculation_begining <- "Ведутся вычисления."
label_calculation_end <- "Вычисления окончены."
width_of_sidebar_panel <- 5
width_of_main_panel <- 7
time_of_notification_duration <- 20

ui <- fluidPage(
  tags$style(HTML("
  /* Базовый размер для всего документа */
  body, .btn, .selectize-input, .selectize-dropdown {
    font-size: 25px !important;
  }

  /* Заголовки вкладок */
  .nav-tabs > li > a {
    font-size: 25px !important;
  }

  /* Таблицы */
  table.dataTable, .dataTables_wrapper {
    font-size: 25px !important;
  }

  /* Заголовки панелей */
  .h4, h4 {
    font-size: 25px !important;
  }
  
  /* Элементы управления вводами */
  .control-label {
    font-size: 25px !important;
  }
  .form-control {
    font-size: 25px !important;
  }
  .input-group-btn.input-group-prepend
  {
    font-size: 25px !important;
  }
  
  /* Кнопка 'Открыть...' */ 
  .btn.btn-default.btn-file
  {
    font-size: 25px !important;
    width: 150px;
  }
  
  /* 'Файл не выбран' */
  .form-control
  {
    font-size: 25px !important;
    height: 49px
  }
  
  .shiny-file-input-progress .progress-bar::after {
    content: Файл загружен
  }
  .progress-bar 
  {
    content: Файл загружен
  }
  
  /* Текст внутри кнопок */
  .action-button {
    font-size: 25px !important;
  }
")),
  titlePanel("Анализ регионов по разным периодам"),
  radioButtons( 
    inputId = "radio", 
    label = label_radio_buttons, 
    choices = list( 
      "TF" = 1, 
      "RAKE" = 2
    )
  ), 
  tabsetPanel(id = "tabs",
              tabPanel("Период 1",
                       sidebarLayout(
                         sidebarPanel(
                           fileInput("file1", label = label_choose_file, 
                                     buttonLabel = label_input_file_button, 
                                     placeholder = label_input_placeholder,
                                     accept = ".xlsx"),
                           actionButton("analyze1", label_action_output_results),
                           downloadButton("downloadData1", 
                                          "Скачать результаты вычислений"),
                           width = width_of_sidebar_panel
                         ),
                         mainPanel(
                           wordcloud2Output("wordcloud1"),
                           plotOutput("barPlot1"),
                           #plotOutput("wordcloud1"),
                           tableOutput("wordTable1"),
                           width = width_of_main_panel
                         )
                       )
              ),
              tabPanel("Период 2",
                       sidebarLayout(
                         sidebarPanel(
                           fileInput("file2", label = label_choose_file, 
                                     buttonLabel = label_input_file_button, 
                                     placeholder = label_input_placeholder,
                                     accept = ".xlsx"),
                           actionButton("analyze2", label_action_output_results),
                           downloadButton("downloadData2", 
                                          "Скачать результаты вычислений"),
                           width = width_of_sidebar_panel
                         ),
                         mainPanel(
                           wordcloud2Output("wordcloud2"),
                           plotOutput("barPlot2"),
                           tableOutput("wordTable2"),
                           width = width_of_main_panel
                         )
                       )
              ),
              tabPanel("Период 3",
                       sidebarLayout(
                         sidebarPanel(
                           fileInput("file3", label = label_choose_file, 
                                     buttonLabel = label_input_file_button, 
                                     placeholder = label_input_placeholder,
                                     accept = ".xlsx"),
                           actionButton("analyze3", label_action_output_results),
                           downloadButton("downloadData3", 
                                          "Скачать результаты вычислений"),
                           width = width_of_sidebar_panel
                         ),
                         mainPanel(
                           wordcloud2Output("wordcloud3"),
                           plotOutput("barPlot3"),
                           tableOutput("wordTable3"), 
                           width = width_of_main_panel
                         )
                       )
              ),
              tabPanel("Период 4",
                       sidebarLayout(
                         sidebarPanel(
                           fileInput("file4", label = label_choose_file, 
                                     buttonLabel = label_input_file_button, 
                                     placeholder = label_input_placeholder,
                                     accept = ".xlsx"),
                           actionButton("analyze4", label_action_output_results),
                           downloadButton("downloadData4", 
                                          "Скачать результаты вычислений"),
                           width = width_of_sidebar_panel
                         ),
                         mainPanel(
                           wordcloud2Output("wordcloud4"),
                           plotOutput("barPlot4"),
                           tableOutput("wordTable4"), 
                           width = width_of_main_panel
                         )
                       )
              ),
              tabPanel("Период 5",
                       sidebarLayout(
                         sidebarPanel(
                           fileInput("file5", label = label_choose_file, 
                                     buttonLabel = label_input_file_button, 
                                     placeholder = label_input_placeholder,
                                     accept = ".xlsx"),
                           actionButton("analyze5", label_action_output_results),
                           downloadButton("downloadData5", 
                                          "Скачать результаты вычислений"),
                           width = width_of_sidebar_panel
                         ),
                         mainPanel(
                           wordcloud2Output("wordcloud5"),
                           plotOutput("barPlot5"),
                           tableOutput("wordTable5"), 
                           width = width_of_main_panel
                         )
                       )
              ),
              tabPanel("Оценка динамики",
                       actionButton("compareFilesBtn", "Сравнить проанализированные файлы"),
                       downloadButton("downloadDataCompare", 
                                      "Скачать результаты вычислений"),
                       tableOutput("compareFilesTable"),
                       plotOutput("dynamicPlotLimited"),
                       plotOutput("dynamicPlotAll"),
                       
              )
  )
)

server <- function(input, output, session) {
  options(shiny.maxRequestSize=30*1024^2)
  files_preprocessed_data_frequency <- reactiveValues()
  files_preprocessed_data_rake <- reactiveValues()
  # Данные для построения облаков слов.
  wordcloud_data_tf_1 <- reactiveVal(NULL)
  wordcloud_data_rake_1 <- reactiveVal(NULL)
  barplot_tf_1 <- reactiveVal(NULL)
  barplot_rake_1 <- reactiveVal(NULL)
  word_table_tf_1 <- reactiveVal(NULL)
  word_table_rake_1 <- reactiveVal(NULL)
  
  wordcloud_data_tf_2 <- reactiveVal(NULL)
  wordcloud_data_rake_2 <- reactiveVal(NULL)
  barplot_tf_2 <- reactiveVal(NULL)
  barplot_rake_2 <- reactiveVal(NULL)
  word_table_tf_2 <- reactiveVal(NULL)
  word_table_rake_2 <- reactiveVal(NULL)
 
  wordcloud_data_tf_3 <- reactiveVal(NULL)
  wordcloud_data_rake_3 <- reactiveVal(NULL)
  barplot_tf_3 <- reactiveVal(NULL)
  barplot_rake_3 <- reactiveVal(NULL)
  word_table_tf_3 <- reactiveVal(NULL)
  word_table_rake_3 <- reactiveVal(NULL)
  
  wordcloud_data_tf_4 <- reactiveVal(NULL)
  wordcloud_data_rake_4 <- reactiveVal(NULL)
  barplot_tf_4 <- reactiveVal(NULL)
  barplot_rake_4 <- reactiveVal(NULL)
  word_table_tf_4 <- reactiveVal(NULL)
  word_table_rake_4 <- reactiveVal(NULL)
  
  wordcloud_data_tf_5 <- reactiveVal(NULL)
  wordcloud_data_rake_5 <- reactiveVal(NULL)
  barplot_tf_5 <- reactiveVal(NULL)
  barplot_rake_5 <- reactiveVal(NULL)
  word_table_tf_5 <- reactiveVal(NULL)
  word_table_rake_5 <- reactiveVal(NULL)
  
  cos_mat_reactive_tf <- reactiveVal(NULL)
  trend_map_reactive_tf <- reactiveVal(NULL)
  cos_mat_reactive_rake <- reactiveVal(NULL)
  trend_map_reactive_rake <- reactiveVal(NULL)  
  
  amount_of_words_in_wordcloud <- 30
  # Удаление лишних пробелов бесполезно, так как их устраняют  при токенизации,
  # Приведение текста к кодировке UTF-8 может быть полезно
  # Удаление цифр полезно
  # Приведение к нижнему регистру полезно, так как оно не происходит 
  # при keywords_rake и udpipe_annotate
  CleanCorpusRake <- function(corpus_to_use){ 
    corpus_to_use %>%
      # tm_map(removePunctuation) %>%
      tm_map(stripWhitespace) %>%
      tm_map(content_transformer(function(x) iconv(x, to='UTF-8'))) %>%
      tm_map(removeNumbers) %>%
      tm_map(content_transformer(tolower)) 
  }
  
  
  
  # все команды этой функции совпадают с соотв-ми командами алгоритма для 14 регионов
  CleanCorpusFrequency <- function(corpus_to_use){  
    corpus_to_use %>%
      tm_map(removePunctuation) %>%
      tm_map(stripWhitespace) %>%
      tm_map(content_transformer(function(x) iconv(x, to='UTF-8'))) %>%
      tm_map(removeNumbers) %>%
      tm_map(content_transformer(tolower)) 
  }
  
  GetRakeKeywords <- function(file) {
    # Проверка на корректность ввода файла. 
    # Если файл введен некорректно, то событие (ObserveEvent), 
    # вызввавшее функцию останавливаетя.
    req(file)
    showNotification(label_calculation_begining, duration = time_of_notification_duration)
    input_data <- as.data.frame(read_excel(file$datapath, col_names = FALSE)) 
    # load_stopwords()
    # Было:
    # corp_city_df <- CleanCorpusFrequency(VCorpus(VectorSource(input_data)))
    # Стало:
    corp_city_df <- CleanCorpusRake(VCorpus(VectorSource(input_data)))
    corp_city_df[["1"]][["content"]] <- 
    gsub("[«№\U{1F600}-\U{1F64F}\U{1F300}-\U{1F5FF}\U{1F680}-\U{1F6FF}\U{1F1E0}-\U{1F1FF}
      \U{2500}-\U{2BEF}\U{2702}-\U{27B0}\U{24C2}-\U{1F251}\U{1f926}-\U{1f937}
      \U{10000}-\U{10ffff}\u{2640}-\u{2642}\u{2600}-\u{2B55}\u{200d}\u{23cf}\u{23e9}
      \u{231a}\u{fe0f}\u{3030}\U{00B0}\U{20BD}]", "", corp_city_df[["1"]][["content"]], perl = TRUE)
    corp_city_df[["1"]][["content"]] <- gsub("\\b\\S*(http|vk)\\S*\\b", "", 
                                        corp_city_df[["1"]][["content"]], perl = TRUE)
    corp_city_df[["1"]][["content"]] <- gsub("движениепервых", "#движение первых#", 
                                             corp_city_df[["1"]][["content"]], perl = TRUE)
    corp_city_df[["1"]][["content"]] <- gsub("навигаторыдетство", "#навигаторы детство#", 
                                             corp_city_df[["1"]][["content"]], perl = TRUE)
    corp_city_df[["1"]][["content"]] <- gsub("новостипервых", "#новости первых#", 
                                             corp_city_df[["1"]][["content"]], perl = TRUE)
    corp_city_df[["1"]][["content"]] <- gsub("фотоомск", "#фото омск#", 
                                             corp_city_df[["1"]][["content"]], perl = TRUE)
    corp_city_df[["1"]][["content"]] <- gsub("большаяучительскаянеделя", 
                        "#большая учительская неделя#", corp_city_df[["1"]][["content"]], perl = TRUE)
    corp_city_df[["1"]][["content"]] <- gsub("годпедагоганаставник", "#год педагога наставник#",
                          corp_city_df[["1"]][["content"]], perl = TRUE)
    corp_city_df[["1"]][["content"]] <- gsub("государственныйсоветреспублики", 
        "#государственный совет республики# ", corp_city_df[["1"]][["content"]], perl = TRUE)
    
    name_of_gsd_model <- 'russian-gsd-ud-2.5-191206.udpipe'
    if (!file.exists(name_of_gsd_model))
    {
      gsd_model_raw <- udpipe_download_model(language = "russian-gsd")
    }
    gsd_model <- udpipe_load_model(file = name_of_gsd_model)
    x <- udpipe_annotate(gsd_model, x = corp_city_df[["1"]][["content"]],  parser = "none")
    x <- as.data.frame(x)   
    tmp <- x$lemma 
    
    tmp <- gsub("[[:punct:]]", "", tmp)
    tmp <- str_replace_all(tmp, 'правительстворазвитие', 'развитие')
    tmp <- str_replace_all(tmp, 'цифровый', 'цифровой')
    tmp <- str_replace_all(tmp, 'научныймощность', 'научный мощность')
    tmp <- str_replace_all(tmp, 'club', '')
    tmp <- str_replace_all(tmp, 'ветр', 'ветер')
    tmp <- str_replace_all(tmp, 'школьник', 'школа')
    tmp <- str_replace_all(tmp, 'школьный', 'школа')
    tmp <- str_replace_all(tmp, 'молние', 'молния')
    
    tmp <- str_replace_all(tmp, 'полицияроссия', 'полиция')
    tmp <- str_replace_all(tmp, 'осуждеть', 'осуждать')
    tmp <- str_replace_all(tmp, 'умвд', 'мвд') 
    tmp <- str_replace_all(tmp, 'юнармеец', 'юнармия') 
    tmp <- str_replace_all(tmp, 'движениепервый', 'движениепервых') 
    tmp <- str_replace_all(tmp, 'перевозкий', 'перевозка') 
    tmp <- str_replace_all(tmp, 'юнармейский', 'юнармия') 
    
    x$lemma <- tmp
    show(x)
    
    keywords_rake_test <- function(x, term, group, relevant = rep(TRUE, nrow(x)), 
                                   ngram_max = 2, n_min = 30, sep = " ") {
      .relevant <- .N <- keyword_id <- keyword <-
      degree <- word <- freq <- ngram <- rake <- rake_word_score <- NULL

      stopifnot(is.data.frame(x))  
      stopifnot(term %in% colnames(x)) 
      stopifnot(all(group %in% colnames(x))) 
      stopifnot(length(relevant) == nrow(x)) 
      if (length(group) > 1) {
        x <- data.table(group = unique_identifier(x, fields = group),
                        word = x[[term]], .relevant = relevant)
      } else {
        x <- data.table(group = x[[group]], word = x[[term]], .relevant = relevant)
      }
      x$keyword_id <- data.table::rleid(x[["group"]], x[[".relevant"]])
      x <- subset(x, .relevant != FALSE, select = c("keyword_id", "word"))
      x <- subset(x, nzchar(trimws(word)))
      x <- x[, `:=`(keyword, paste(word, collapse = sep)), by = list(keyword_id)]
      x <- x[, `:=`(degree, .N - 1L), by = list(keyword_id)]
      word_score <- list()
      word_score$degree <- x[, list(degree = sum(degree)), by = list(word)]
      word_score$freq <- txt_freq(x$word)
      word_score$freq <- setDT(word_score$freq) 
      word_score <- merge(word_score$degree, word_score$freq,
                          by.x = "word", by.y = "key", all.x = FALSE, all.y = TRUE)
      word_score$rake_word_score <- word_score$degree / word_score$freq
      show(x)
      show(word_score)
      keywords <- x[, list(freq = length(unique(keyword_id))), by = list(keyword, word)]
      keywords <- merge(keywords, word_score[, c("word", "rake_word_score"), with = FALSE],
                        by = "word", all.x = TRUE, all.y = FALSE)
      show(keywords)
      keywords <- keywords[, list(
        ngram = sapply(strsplit(keyword[1], sep, fixed = TRUE), length),
        rake = sum(rake_word_score)
      ), by = list(keyword, freq)]
      
      show(keywords)
      for(keyword in keywords$keyword)
      {
        if (str_detect(keyword, "движение")){
          show(keyword)
        }
      }
      # Фильтрация результатов по максимальному ngram и минимальной частоте
      keywords <- subset(keywords, ngram <= ngram_max & freq >= n_min)
      exclude_phrases <- c("первый", 'президентский нацпроект',
       'активист движение', 'почетный гость', 'электронный почта', 
       'приятный поездка', 'малый класс', 'социальный сеть', 
       'юнармия', 'отряд', 'детский', 'поздравление первый')
      keywords <- keywords[!keyword %in% exclude_phrases, ]
      # Сортировка результатов по убыванию RAKE
      setorder(keywords, -rake)
      # Переупорядочивание столбцов: keyword, ngram, freq, rake
      keywords <- data.table::setcolorder(keywords, neworder = c("keyword", "ngram", "freq", "rake"))
      # Преобразование результата в data.frame для вывода
      keywords <- setDF(keywords)
      # Возврат итогового data.frame
      keywords
    }
    
    min_freq_of_phrase <- 10
    keywords_rake_df <- keywords_rake_test(x, term = "lemma", group = c("sentence_id"),
                                      relevant = x$upos %in% c("NOUN", "ADJ") &
                                        !(x$lemma %in% stopwords_combined_list),
                                      ngram_max = 3, n_min = min_freq_of_phrase)
    return(keywords_rake_df)
  }
  
  GetPreprocessedTextsWordList <- function(file) {   
    req(file)
    showNotification(label_calculation_begining, duration = time_of_notification_duration)
    input_data <- as.data.frame(read_excel(file$datapath, col_names = FALSE)) 
    corp_city_df <- CleanCorpusFrequency(VCorpus(VectorSource(input_data)))
    corp_city_df[["1"]][["content"]] <- gsub("[\U{1F600}-\U{1F64F}\U{1F300}-\U{1F5FF}
      \U{1F680}-\U{1F6FF}\U{1F1E0}-\U{1F1FF}\U{2500}-\U{2BEF}\U{2702}-\U{27B0}
      \U{24C2}-\U{1F251}\U{1f926}-\U{1f937}\U{10000}-\U{10ffff}\u{2640}-\u{2642}\u{2600}-\u{2B55}
      \u{200d}\u{23cf}\u{23e9}\u{231a}\u{fe0f}\u{3030}\U{00B0}\U{20BD}]", "", 
                                             corp_city_df[["1"]][["content"]], perl = TRUE)
    corp_city_df[["1"]][["content"]] <- gsub("\\b\\S*(http|vk)\\S*\\b", "",
                                             corp_city_df[["1"]][["content"]], perl = TRUE)
    corp_city_df[["1"]][["content"]] <- gsub("движениепервых", "движение первых", 
                                             corp_city_df[["1"]][["content"]], perl = TRUE)
    corp_city_df[["1"]][["content"]] <- gsub("навигаторыдетство", "навигаторы детство", 
                                             corp_city_df[["1"]][["content"]], perl = TRUE)
    corp_city_df[["1"]][["content"]] <- gsub("новостипервых", "новости первых", 
                                             corp_city_df[["1"]][["content"]], perl = TRUE)
    corp_city_df[["1"]][["content"]] <- gsub("фотоомск", "фото омск", 
                                             corp_city_df[["1"]][["content"]], perl = TRUE)
    corp_city_df[["1"]][["content"]] <- gsub("большаяучительскаянеделя", "большая учительская неделя", 
                                             corp_city_df[["1"]][["content"]], perl = TRUE)
    corp_city_df[["1"]][["content"]] <- gsub("годпедагоганаставник", "год педагога наставник", 
                                             corp_city_df[["1"]][["content"]], perl = TRUE)
    corp_city_df[["1"]][["content"]] <- gsub("государственныйсоветреспублики", 
                          "государственный совет республики ", corp_city_df[["1"]][["content"]], perl = TRUE)
    
    if (!file.exists('russian-gsd-ud-2.5-191206.udpipe'))
    {
      gsd_model_raw <- udpipe_download_model(language = "russian-gsd")
    }
    gsd_model <- udpipe_load_model(file = 'russian-gsd-ud-2.5-191206.udpipe')
    x <- udpipe_annotate(gsd_model, x = corp_city_df[["1"]][["content"]],  parser = "none")
    x <- as.data.frame(x)  
    x$lemma <- noquote(x$lemma)
    x$lemma <- str_replace_all(x$lemma, "[[:punct:]]", "")
    tmp <- x$lemma
    tmp <- str_replace_all(x$lemma, paste("\\b(", stopwords_combined_str, ")\\b"), "")
    tmp <- str_replace_all(tmp, '№', '')
    tmp <- str_replace_all(tmp, '−', '')
    tmp <- str_replace_all(tmp, '—', '')
    tmp <- str_replace_all(tmp, 'правительстворазвитие', 'развитие')
    tmp <- str_replace_all(tmp, 'цифровый', 'цифровой')
    tmp <- str_replace_all(tmp, 'научныймощность', 'научный мощность')
    tmp <- str_replace_all(tmp, 'club', '')
    tmp <- str_replace_all(tmp, 'ветр', 'ветер')
    tmp <- str_replace_all(tmp, 'школьник', 'школа')
    tmp <- str_replace_all(tmp, 'школьный', 'школа')
    tmp <- str_replace_all(tmp, 'молние', 'молния')
    tmp <- str_replace_all(tmp, 'полицияроссия', 'полиция')
    tmp <- str_replace_all(tmp, 'осуждеть', 'осуждать')
    tmp <- str_replace_all(tmp, 'умвд', 'мвд') 
    tmp <- str_replace_all(tmp, 'юнармеец', 'юнармия') 
    tmp <- str_replace_all(tmp, 'движениепервый', 'движениепервых')
    tmp <- str_replace_all(tmp, 'перевозкий', 'перевозка') 
    tmp <- str_replace_all(tmp, 'юнармейский', 'юнармия') 
    tmp <- str_replace_all(tmp, 'первый', '') 
    tmp <- tmp[sapply(tmp, nchar) > 0]
    return(tmp)
  }
  
  AnalyzeAndRenderRake <-  function(file_input, id_plot_output, id_table_output, id_wordcloud_output) { 
    keywords_rake_df <- GetRakeKeywords(file_input)
    keywords_rake_df_for_output <- keywords_rake_df[c("keyword", "freq", "rake")]
    name_of_input_file <- file_input$name
    output[[id_plot_output]] <- renderPlot({
      diagram_for_output <- ggplot(keywords_rake_df_for_output[1:10, ], aes(x = reorder(keyword, rake), y = rake)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        labs(title = "Слова с наибольшим индексом RAKE", x = "Слова", y = "Индекс RAKE") +
        theme_gray(base_size = 26)
      if (id_plot_output == "barPlot1")
        barplot_rake_1(diagram_for_output)
      else if (id_plot_output == "barPlot2")
        barplot_rake_2(diagram_for_output)
      else if (id_plot_output == "barPlot3")
        barplot_rake_3(diagram_for_output)
      else if (id_plot_output == "barPlot4")
        barplot_rake_4(diagram_for_output)
      else if (id_plot_output == "barPlot5")
        barplot_rake_5(diagram_for_output)
      diagram_for_output
    })
    output[[id_table_output]] <- renderTable({
      colnames(keywords_rake_df_for_output) <- c("Ключевые слова", "Частота встречаемости", "RAKE")
      table_rake_for_output <- head(keywords_rake_df_for_output, 10)
      if (id_table_output == "wordTable1")
        word_table_rake_1(table_rake_for_output)
      else if (id_table_output == "wordTable2")
        word_table_rake_2(table_rake_for_output)
      else if (id_table_output == "wordTable3")
        word_table_rake_3(table_rake_for_output)
      else if (id_table_output == "wordTable4")
        word_table_rake_4(table_rake_for_output)
      else if (id_table_output == "wordTable5")
        word_table_rake_5(table_rake_for_output)
      table_rake_for_output
    })
    keywords_rake_df_for_output_wordcloud <- keywords_rake_df_for_output[c("keyword", "freq")]
    # Сортировка по столбцу freq по убыванию для облака слов
    keywords_rake_df_for_output_wordcloud <- keywords_rake_df_for_output_wordcloud[
      order(keywords_rake_df_for_output_wordcloud$freq, decreasing = TRUE),]
    output[[id_wordcloud_output]] <- renderWordcloud2({
      wordcloud_rake_for_output <- head(keywords_rake_df_for_output_wordcloud, amount_of_words_in_wordcloud)
      if (id_wordcloud_output == "wordcloud1")
        wordcloud_data_rake_1(wordcloud_rake_for_output) 
      else if (id_wordcloud_output == "wordcloud2")
        wordcloud_data_rake_2(wordcloud_rake_for_output) 
      else if (id_wordcloud_output == "wordcloud3")
        wordcloud_data_rake_3(wordcloud_rake_for_output) 
      else if (id_wordcloud_output == "wordcloud4")
        wordcloud_data_rake_4(wordcloud_rake_for_output) 
      else if (id_wordcloud_output == "wordcloud5")
        wordcloud_data_rake_5(wordcloud_rake_for_output) 
      Wordcloud2a(wordcloud_rake_for_output, size = 0.45)
    })
    showNotification(label_calculation_end, duration = time_of_notification_duration)
    return(keywords_rake_df)
  }
  
  AnalyzeAndRenderFrequency <- function(file_input, id_plot_output, id_table_output, id_wordcloud_output) {   
    preprocessed_texts_word_list <- GetPreprocessedTextsWordList(file_input)
    d <- as.data.frame(sort(table(preprocessed_texts_word_list), decreasing = TRUE))
    # show(d)
    colnames(d) <- c("word", "freq")
    word_freq <- d
    all_freq_for_document <- sum(d$freq)
    # show(all_freq_for_document)
    d$tf <- d$freq / all_freq_for_document
    d_word_tf <- select(d, c("word", "tf"))
    
    
    output[[id_plot_output]]  <- renderPlot({
      barplot_tf <- ggplot(d_word_tf[1:10, ], aes(x = reorder(word, tf), y = tf)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        labs(title = "Ключевые слова", x = "Слова", y = "Term Frequency") +
        theme_gray(base_size = 26)
      name_of_input_file <- file_input$name
      
      if (id_plot_output == "barPlot1")
        barplot_tf_1(barplot_tf)
      else if (id_plot_output == "barPlot2")
        barplot_tf_2(barplot_tf)
      else if (id_plot_output == "barPlot3")
        barplot_tf_3(barplot_tf)
      else if (id_plot_output == "barPlot4")
        barplot_tf_4(barplot_tf)
      else if (id_plot_output == "barPlot5")
        barplot_tf_5(barplot_tf)
      barplot_tf
    })
    
    output[[id_table_output]] <- renderTable({
      colnames(d_word_tf) <- c("Слово", "TF")
      table_rake_for_output <- head(d_word_tf, 10)
      if (id_table_output == "wordTable1")
        word_table_tf_1(table_rake_for_output)
      else if (id_table_output == "wordTable2")
        word_table_tf_2(table_rake_for_output)
      else if (id_table_output == "wordTable3")
        word_table_tf_3(table_rake_for_output)
      else if (id_table_output == "wordTable4")
        word_table_tf_4(table_rake_for_output)
      else if (id_table_output == "wordTable5")
        word_table_tf_5(table_rake_for_output)
      table_rake_for_output
    }, digits = 4
    )
    output[[id_wordcloud_output]] <- renderWordcloud2({
      wordcloud_tf_for_output <- head(d_word_tf, amount_of_words_in_wordcloud)
      wordcloud_data_tf_1(wordcloud_tf_for_output)
      
      if (id_wordcloud_output == "wordcloud1")
        wordcloud_data_tf_1(wordcloud_tf_for_output)
      else if (id_wordcloud_output == "wordcloud2")
        wordcloud_data_tf_2(wordcloud_tf_for_output)
      else if (id_wordcloud_output == "wordcloud3")
        wordcloud_data_tf_3(wordcloud_tf_for_output)
      else if (id_wordcloud_output == "wordcloud4")
        wordcloud_data_tf_4(wordcloud_tf_for_output)
      else if (id_wordcloud_output == "wordcloud5")
        wordcloud_data_tf_5(wordcloud_tf_for_output)
      Wordcloud2a(head(d_word_tf, amount_of_words_in_wordcloud), size = 0.45)
    })
    showNotification(label_calculation_end, duration = time_of_notification_duration)
    show(d)
    return(d)
  }
  ObserveEventCompareFilesBtnFrequency <- function(){
    d_all <- Filter(Negate(is.null), list(files_preprocessed_data_frequency[["df_1"]], 
                                          files_preprocessed_data_frequency[["df_2"]], 
                                          files_preprocessed_data_frequency[["df_3"]],
                                          files_preprocessed_data_frequency[["df_4"]],
                                          files_preprocessed_data_frequency[["df_5"]])) 
    cos.mat <- NULL
    if (length(d_all) <= 1) {
      showNotification("Для анализа должно быть обработано не менее двух файлов с помощью одного метода.", 
                       duration = time_of_notification_duration)
    }
    else 
    {  
      showNotification(label_calculation_begining, duration = time_of_notification_duration)
      amount_of_processed_files <- length(d_all)
      for (i in  1:amount_of_processed_files)
      {
        names(d_all[[i]]) <- c("word", paste("freq", i, sep = ""), paste("tf", i, sep = ""))
      }
      res <- d_all[[1]]
      # Соединение таблиц по столбцу word
      for (i in  2:amount_of_processed_files)
      {
        res <- full_join(res, d_all[[i]], by = 'word')
      }
      d_all <- res
      # Замена NA на 0
      d_all <- d_all %>% replace(is.na (.), 0)
      col_names_word_freq_tf <- c('word')
      col_names_word_freq <- c('word')
      col_names_freq <- c()
      for (i in 1:amount_of_processed_files)
      {
        col_names_word_freq_tf <- c(col_names_word_freq_tf,  paste("freq", i, sep = ""), paste("tf", i, sep = ""))
        col_names_word_freq <- c(col_names_word_freq, paste("freq", i, sep = ""))
        col_names_freq <- c(col_names_freq, paste("freq", i, sep = ""))
      }
      tf_idf <- d_all %>% select(all_of(col_names_word_freq_tf))
      tdm_df <- d_all %>% select(all_of(col_names_word_freq))
      tdm_df <- tdm_df %>% mutate(num_of_occurrences = rowSums(select(tdm_df,
                                                                      all_of(col_names_freq)) != 0))
      tdm_df <- tdm_df %>% mutate(idf = log((amount_of_processed_files + 1) / 
                                              (1 + num_of_occurrences) + 1))
      tdm_df_with_dynamism <- tdm_df
      tdm_df_with_dynamism$freq_all <- tdm_df_with_dynamism[['freq1']]
      for (i in 2:amount_of_processed_files)
      {
        tdm_df_with_dynamism$freq_all <- tdm_df_with_dynamism$freq_all +
          tdm_df_with_dynamism[[paste("freq", i, sep = "")]]
      }
      tdm_df_with_dynamism$dynamism <- tdm_df_with_dynamism[[paste("freq", amount_of_processed_files, sep = "")]] -
        tdm_df_with_dynamism[["freq1"]]
      tdm_df_with_dynamism$dynamism <- tdm_df_with_dynamism$dynamism / (amount_of_processed_files - 1)
      tf_idf <- tf_idf %>% mutate(num_of_occurrences = tdm_df$num_of_occurrences)
      tf_idf <- tf_idf %>% mutate(idf = tdm_df$idf)
      col_names_tf <- c()
      col_names_tfidf <- c()
      for (i in 1:amount_of_processed_files)
      {
        col_name_tf <- paste0("tf", i, sep = "")
        col_names_tf <- c(col_names_tf, col_name_tf)
        col_name_tfidf <- paste0("tf_idf", i, sep = "")
        col_names_tfidf <- c(col_names_tfidf, col_name_tfidf)
        tf_idf <- tf_idf %>% mutate(!!col_name_tfidf :=
                                      .data[[col_name_tf]] * idf)
      }
      tf_idf_only <- select(tf_idf, all_of(col_names_tfidf))
      period_names <- c()
      for (i in 1:amount_of_processed_files)
      {
        period_names <- c(period_names, paste0("Период ", i, sep = ""))
      }
      names(tf_idf_only) <- period_names
      cos.mat <- cosine(as.matrix(tf_idf_only))  # Removes the first column for cosine calculation
      tdm_df_with_dynamism$freq_all_normalized <- (tdm_df_with_dynamism$freq_all) / ifelse(max(tdm_df_with_dynamism$freq_all) != 0, max(tdm_df_with_dynamism$freq_all), 1)  
      tdm_df_with_dynamism$dynamism_normalized <- (tdm_df_with_dynamism$dynamism) / ifelse(max(tdm_df_with_dynamism$dynamism) != 0, max(tdm_df_with_dynamism$dynamism), 1)  
      tdm_df_with_dynamism$freq_all_and_dynamism_normalized <- tdm_df_with_dynamism$dynamism_normalized + tdm_df_with_dynamism$freq_all_normalized
      # Сортировка датафрейма по столбцу freq_all_and_dynamism_normalized по убыванию
      tdm_df_with_dynamism <- tdm_df_with_dynamism[order(tdm_df_with_dynamism$freq_all_and_dynamism_normalized, decreasing = TRUE),] 
      output$compareFilesTable <- renderTable({
        cos_mat_reactive_tf(cos.mat)
        cos.mat
      })
      output$dynamicPlotLimited <- renderPlot({
        amount_of_words_in_plot <- 10 
        tdm_df_with_dynamism_limited <- tdm_df_with_dynamism[1:amount_of_words_in_plot, ]
        tdm_df_with_dynamism_limited$dynamism_shifted_for_30 <- tdm_df_with_dynamism_limited$dynamism - 
          min(tdm_df_with_dynamism_limited$dynamism)
        tdm_df_with_dynamism_limited$dynamism_normalized_for_30 <- tdm_df_with_dynamism_limited$dynamism_shifted_for_30 /
          ifelse(max(tdm_df_with_dynamism_limited$dynamism_shifted_for_30) != 0,
                 max(tdm_df_with_dynamism_limited$dynamism_shifted_for_30), 1)
        tdm_df_with_dynamism_limited$freq_all_shifted_for_30 <- tdm_df_with_dynamism_limited$freq_all - min(tdm_df_with_dynamism_limited$freq_all)
        tdm_df_with_dynamism_limited$freq_all_normalized_for_30 <- (tdm_df_with_dynamism_limited$freq_all_shifted_for_30) /
          ifelse(max(tdm_df_with_dynamism_limited$freq_all_shifted_for_30) != 0,
                 max(tdm_df_with_dynamism_limited$freq_all_shifted_for_30), 1)
        dynamicPlotLimited <- ggplot(tdm_df_with_dynamism_limited[1:amount_of_words_in_plot, ], aes(x = dynamism_normalized_for_30, y = freq_all_normalized_for_30, label = word)) +
          geom_point() +
          # geom_label_repel(max.overlaps = 10, label.size = 10) +
          geom_text_repel(max.overlaps = 10, size = 7) +
          labs(x = "Динамика", y = "Значимость", title = paste0("Тренд-карта")) +
          theme_classic(base_size = 26)
        trend_map_reactive_tf(dynamicPlotLimited)
        showNotification(label_calculation_end, duration = time_of_notification_duration)
        return(dynamicPlotLimited)
      })
    }
  }
  ObserveEventCompareFilesBtnRake <- function() {
    d_all <- Filter(Negate(is.null), list(files_preprocessed_data_rake[["df_1"]],
                                          files_preprocessed_data_rake[["df_2"]],
                                          files_preprocessed_data_rake[["df_3"]],
                                          files_preprocessed_data_rake[["df_4"]],
                                          files_preprocessed_data_rake[["df_5"]]))
    cos.mat <- NULL
    if (length(d_all) <= 1) {
      showNotification("Для анализа должно быть обработано не менее двух файлов с помощью одного метода.",
                       duration = time_of_notification_duration)
    }
    else
    {
      showNotification(label_calculation_begining, duration = time_of_notification_duration)
      amount_of_processed_files <- length(d_all)
      for (i in  1:amount_of_processed_files)
      {
        names(d_all[[i]]) <- c("keyword", paste("ngram", i, sep = ''), paste("freq", i, sep = ""), paste("rake", i, sep = ""))
      }
      res <- d_all[[1]]
      # Соединение таблиц по столбцу keyword
      for (i in  2:amount_of_processed_files)
      {
        res <- full_join(res, d_all[[i]], by = 'keyword')
      }
      d_all <- res
      d_all <- d_all %>% replace(is.na (.), 0)
      # col_names_keyword_freq_tf <- c('keyword')
      col_names_keyword_rake<- c('keyword')
      col_names_rake <- c()
      for (i in 1:amount_of_processed_files)
      {
        col_names_keyword_rake <- c(col_names_keyword_rake, paste("rake", i, sep = ""))
        col_names_rake <- c(col_names_rake, paste("rake", i, sep = ""))
      }
      rake_df <- select(d_all, 'keyword', all_of(col_names_keyword_rake))
      rake_df_only <- select(d_all, all_of(col_names_rake))
      period_names <- c()
      for (i in 1:amount_of_processed_files)
      {
        period_names <- c(period_names, paste0("Период ", i, sep = ""))
      }
      names(rake_df_only) <- period_names
      cos.mat <- cosine(as.matrix(rake_df_only))
      rake_df_with_dynamism <- rake_df
      rake_df_with_dynamism$rake_all <- rake_df_with_dynamism[['rake1']]
      for (i in 2:amount_of_processed_files)
      {
        rake_df_with_dynamism$rake_all <- rake_df_with_dynamism$rake_all +
          rake_df_with_dynamism[[paste("rake", i, sep = "")]]
      }
      # Средний абсолютный прирост
      rake_df_with_dynamism$dynamism <- rake_df_with_dynamism[[paste("rake", amount_of_processed_files, sep = "")]]
      for (i in (amount_of_processed_files - 1):1)
      {
        rake_df_with_dynamism$dynamism <- (rake_df_with_dynamism$dynamism -
          rake_df_with_dynamism[["rake1"]]) / (amount_of_processed_files - 1)
      }
      rake_df_with_dynamism$rake_all_normalized <- (rake_df_with_dynamism$rake_all) /
        ifelse(max(rake_df_with_dynamism$rake_all) != 0,
               max(rake_df_with_dynamism$rake_all), 1)

      value_for_norm_of_dynamic <- ifelse(min(rake_df_with_dynamism$dynamism) < 0,
                                          -min(rake_df_with_dynamism$dynamism), 0)
      rake_df_with_dynamism$dynamism_normalized <- (rake_df_with_dynamism$dynamism +
                                                      value_for_norm_of_dynamic) /
        ifelse(max(rake_df_with_dynamism$dynamism + value_for_norm_of_dynamic) != 0,
               max(rake_df_with_dynamism$dynamism + value_for_norm_of_dynamic), 1)
      rake_df_with_dynamism$sum_of_rake_all_norm_and_dyn_norm <-
        rake_df_with_dynamism$dynamism_normalized + rake_df_with_dynamism$rake_all_normalized
      show(rake_df_with_dynamism)
      # Сортировка датафрейма по столбцу freq_all_and_dynamism_normalized по убыванию
      rake_df_with_dynamism <- rake_df_with_dynamism[order(rake_df_with_dynamism$sum_of_rake_all_norm_and_dyn_norm, decreasing = TRUE),]
      output$compareFilesTable <- renderTable({
        cos_mat_reactive_rake(cos.mat)
        cos.mat
      })
      output$dynamicPlotLimited <- renderPlot({
        amount_of_words_in_plot <- 10
        rake_df_with_dynamism_limited <- rake_df_with_dynamism[1:amount_of_words_in_plot, ]
        rake_df_with_dynamism_limited$dynamism_shifted_for_30 <- rake_df_with_dynamism_limited$dynamism -
          min(rake_df_with_dynamism_limited$dynamism)
        rake_df_with_dynamism_limited$dynamism_normalized_for_30 <- rake_df_with_dynamism_limited$dynamism_shifted_for_30 /
          ifelse(max(rake_df_with_dynamism_limited$dynamism_shifted_for_30) != 0,
                 max(rake_df_with_dynamism_limited$dynamism_shifted_for_30), 1)
        rake_df_with_dynamism_limited$rake_all_shifted_for_30 <- rake_df_with_dynamism_limited$rake_all - min(rake_df_with_dynamism_limited$rake_all)
        rake_df_with_dynamism_limited$rake_all_normalized_for_30 <- (rake_df_with_dynamism_limited$rake_all_shifted_for_30) /
          ifelse(max(rake_df_with_dynamism_limited$rake_all_shifted_for_30) != 0,
                 max(rake_df_with_dynamism_limited$rake_all_shifted_for_30), 1)
        plot_limited <- ggplot(rake_df_with_dynamism_limited, aes(x = dynamism_normalized_for_30, y = rake_all_normalized_for_30, label = keyword)) +
          geom_point() +
          geom_text_repel(max.overlaps = 10, size = 7) +
          labs(x = "Динамика", y = "Значимость", title = paste0("Тренд-карта")) +
          theme_classic(base_size = 26)
        trend_map_reactive_rake(plot_limited)
        showNotification(label_calculation_end, duration = time_of_notification_duration)
        return(plot_limited)
      })
    }
  }
  output$downloadData1 <- downloadHandler(
    filename = function() {
      paste("data1-", Sys.Date(), ".zip", sep = "")
    },
    content = function(file) {
      tmpdir <- tempdir()
      oldwd <- setwd(tmpdir)
      on.exit(setwd(oldwd))
      files_to_zip <- character(0)
      valid_files <- FALSE
      
      tryCatch({
        if (input$radio == 2) {
          if (!is.null(wordcloud_data_rake_1()) && 
              !is.null(barplot_rake_1()) && 
              !is.null(word_table_rake_1())) {
            f1_rake <- "wordcloud_rake.png"
            f2_rake <- "barPlot_rake.png"
            f3_rake <- "table_rake.csv"
            png(f1_rake, width = 800, height = 600)
            print(wordcloud(wordcloud_data_rake_1()[["keyword"]], wordcloud_data_rake_1()[["freq"]], min.freq = 0))
            dev.off()
            png(f2_rake, width = 800, height = 600)
            print(barplot_rake_1())
            dev.off()
            write.csv(word_table_rake_1(), f3_rake, row.names = FALSE)
            files_to_zip <- c(f1_rake, f2_rake, f3_rake)
            valid_files <- TRUE
          }
        } 
        else if (input$radio == 1) {
          if (!is.null(wordcloud_data_tf_1()) && 
              !is.null(barplot_tf_1()) && 
              !is.null(word_table_tf_1())) {
            f1_tf <- "wordcloud_tf.png"
            f2_tf <- "barPlot_tf.png"
            f3_tf <- "table_tf.csv"
            png(f1_tf, width = 800, height = 600)
            print(wordcloud(wordcloud_data_tf_1()[["word"]], wordcloud_data_tf_1()[["tf"]], , min.freq = 0))
            dev.off()
            png(f2_tf, width = 800, height = 600)
            print(barplot_tf_1())
            dev.off()
            write.csv(word_table_tf_1(), f3_tf, row.names = FALSE)
            files_to_zip <- c(f1_tf, f2_tf, f3_tf)
            valid_files <- TRUE
          }
        }
        if (valid_files && length(files_to_zip) > 0) {
          existing_files <- files_to_zip[file.exists(files_to_zip)]
          if (length(existing_files) > 0) {
            zip::zip(zipfile = file, files = existing_files)
          } else {
            stop("Файлы для архивации не были созданы")
          }
        } else {
          stop("Нет данных для экспорта. Сначала создайте таблицу и графики.")
        }
      }, error = function(e) {
        stop(sprintf("Ошибка при создании архива: %s", e$message))
      })
    }
  )
  
  output$downloadData2 <- downloadHandler(
    filename = function() {
      paste("data2-", Sys.Date(), ".zip", sep="")
    },
    content = function(file) {
      tmpdir <- tempdir()
      oldwd <- setwd(tmpdir)
      on.exit(setwd(oldwd))
      files_to_zip <- character(0)
      valid_files <- FALSE
      tryCatch({
        if (input$radio == 2) {
          if (!is.null(wordcloud_data_rake_2()) && 
              !is.null(barplot_rake_2()) && 
              !is.null(word_table_rake_2())) {
            f1_rake <- "wordcloud_rake.png"
            f2_rake <- "barPlot_rake.png"
            f3_rake <- "table_rake.csv"
            png(f1_rake, width = 800, height = 600)
            print(wordcloud(wordcloud_data_rake_2()[["keyword"]], wordcloud_data_rake_2()[["freq"]], min.freq = 0))
            dev.off()
            png(f2_rake, width = 800, height = 600)
            print(barplot_rake_2())
            dev.off()
            write.csv(word_table_rake_2(), f3_rake, row.names = FALSE)
            files_to_zip <- c(f1_rake, f2_rake, f3_rake)
            valid_files <- TRUE
          }
        } 
        else if (input$radio == 1) {
          if (!is.null(wordcloud_data_tf_2()) && 
              !is.null(barplot_tf_2()) && 
              !is.null(word_table_tf_2())) {
            f1_tf <- "wordcloud_tf.png"
            f2_tf <- "barPlot_tf.png"
            f3_tf <- "table_tf.csv"
            png(f1_tf, width = 800, height = 600)
            print(wordcloud(wordcloud_data_tf_2()[["word"]], wordcloud_data_tf_2()[["tf"]], min.freq = 0))
            dev.off()
            png(f2_tf, width = 800, height = 600)
            print(barplot_tf_2())
            dev.off()
            write.csv(word_table_tf_2(), f3_tf, row.names = FALSE)
            files_to_zip <- c(f1_tf, f2_tf, f3_tf)
            valid_files <- TRUE
          }
        }
        if (valid_files && length(files_to_zip) > 0) {
          existing_files <- files_to_zip[file.exists(files_to_zip)]
          if (length(existing_files) > 0) {
            zip::zip(zipfile = file, files = existing_files)
          } else {
            stop("Файлы для архивации не были созданы")
          }
        } else {
          stop("Нет данных для экспорта. Сначала создайте таблицу и графики.")
        }
      }, error = function(e) {
        stop(sprintf("Ошибка при создании архива: %s", e$message))
      })
    }
  )
  output$downloadData3 <- downloadHandler(
    filename = function() {
      paste("data3-", Sys.Date(), ".zip", sep="")
    },
    content = function(file) {
      tmpdir <- tempdir()
      oldwd <- setwd(tmpdir)
      on.exit(setwd(oldwd))
      
      files_to_zip <- character(0)
      valid_files <- FALSE
      
      tryCatch({
        if (input$radio == 2) {

          if (!is.null(wordcloud_data_rake_3()) && 
              !is.null(barplot_rake_3()) && 
              !is.null(word_table_rake_3())) {
            f1_rake <- "wordcloud_rake.png"
            f2_rake <- "barPlot_rake.png"
            f3_rake <- "table_rake.csv"
            png(f1_rake, width = 800, height = 600)
            print(wordcloud(wordcloud_data_rake_3()[["keyword"]], wordcloud_data_rake_3()[["freq"]], min.freq = 0))
            dev.off()
            png(f2_rake, width = 800, height = 600)
            print(barplot_rake_3())
            dev.off()
            write.csv(word_table_rake_3(), f3_rake, row.names = FALSE)
            files_to_zip <- c(f1_rake, f2_rake, f3_rake)
            valid_files <- TRUE
          }
        } 
        else if (input$radio == 1) {
          if (!is.null(wordcloud_data_tf_3()) && 
              !is.null(barplot_tf_3()) && 
              !is.null(word_table_tf_3())) {
            f1_tf <- "wordcloud_tf.png"
            f2_tf <- "barPlot_tf.png"
            f3_tf <- "table_tf.csv"
            png(f1_tf, width = 800, height = 600)
            print(wordcloud(wordcloud_data_tf_3()[["word"]], wordcloud_data_tf_3()[["tf"]], min.freq = 0))
            dev.off()
            png(f2_tf, width = 800, height = 600)
            print(barplot_tf_3())
            dev.off()
            write.csv(word_table_tf_3(), f3_tf, row.names = FALSE)
            files_to_zip <- c(f1_tf, f2_tf, f3_tf)
            valid_files <- TRUE
          }
        }
        
        if (valid_files && length(files_to_zip) > 0) {
          existing_files <- files_to_zip[file.exists(files_to_zip)]
          if (length(existing_files) > 0) {
            zip::zip(zipfile = file, files = existing_files)
          } else {
            stop("Файлы для архивации не были созданы")
          }
        } else {
          stop("Нет данных для экспорта. Сначала создайте таблицу и графики.")
        }
      }, error = function(e) {
        stop(sprintf("Ошибка при создании архива: %s", e$message))
      })
    }
  )
  output$downloadData4 <- downloadHandler(
    filename = function() {
      paste("data4-", Sys.Date(), ".zip", sep="")
    },
    content = function(file) {
      # Создаем временную директорию
      tmpdir <- tempdir()
      oldwd <- setwd(tmpdir)
      on.exit(setwd(oldwd))
      files_to_zip <- character(0)
      valid_files <- FALSE
      tryCatch({
        if (input$radio == 2) {
          if (!is.null(wordcloud_data_rake_4()) && 
              !is.null(barplot_rake_4()) && 
              !is.null(word_table_rake_4())) {
            f1_rake <- "wordcloud_rake.png"
            f2_rake <- "barPlot_rake.png"
            f3_rake <- "table_rake.csv"
            png(f1_rake, width = 800, height = 600)
            print(wordcloud(wordcloud_data_rake_4()[["keyword"]], wordcloud_data_rake_4()[["freq"]], min.freq = 0))
            dev.off()
            png(f2_rake, width = 800, height = 600)
            print(barplot_rake_4())
            dev.off()
            write.csv(word_table_rake_4(), f3_rake, row.names = FALSE)
            files_to_zip <- c(f1_rake, f2_rake, f3_rake)
            valid_files <- TRUE
          }
        } 
        else if (input$radio == 1) {
          # Проверяем наличие данных для TF
          if (!is.null(wordcloud_data_tf_4()) && 
              !is.null(barplot_tf_4()) && 
              !is.null(word_table_tf_4())) {
            f1_tf <- "wordcloud_tf.png"
            f2_tf <- "barPlot_tf.png"
            f3_tf <- "table_tf.csv"
            png(f1_tf, width = 800, height = 600)
            print(wordcloud(wordcloud_data_tf_4()[["word"]], wordcloud_data_tf_4()[["tf"]], min.freq = 0))
            dev.off()
            png(f2_tf, width = 800, height = 600)
            print(barplot_tf_4())
            dev.off()
            write.csv(word_table_tf_4(), f3_tf, row.names = FALSE)
            files_to_zip <- c(f1_tf, f2_tf, f3_tf)
            valid_files <- TRUE
          }
        }
        
        if (valid_files && length(files_to_zip) > 0) {
          existing_files <- files_to_zip[file.exists(files_to_zip)]
          
          if (length(existing_files) > 0) {
            zip::zip(zipfile = file, files = existing_files)
          } else {
            stop("Файлы для архивации не были созданы")
          }
        } else {
          stop("Нет данных для экспорта. Сначала создайте таблицу и графики.")
        }
      }, error = function(e) {
        stop(sprintf("Ошибка при создании архива: %s", e$message))
      })
    }
  )
  output$downloadData5 <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".zip", sep="")
    },
    content = function(file) {
      tmpdir <- tempdir()
      oldwd <- setwd(tmpdir)
      on.exit(setwd(oldwd))
      files_to_zip <- character(0)
      valid_files <- FALSE
      tryCatch({
        if (input$radio == 2) {
          if (!is.null(wordcloud_data_rake_5()) && 
              !is.null(barplot_rake_5()) && 
              !is.null(word_table_rake_5())) {
            f1_rake <- "wordcloud_rake.png"
            f2_rake <- "barPlot_rake.png"
            f3_rake <- "table_rake.csv"
            png(f1_rake, width = 800, height = 600)
            print(wordcloud(wordcloud_data_rake_5()[["keyword"]], wordcloud_data_rake_5()[["freq"]], min.freq = 0))
            dev.off()
            png(f2_rake, width = 800, height = 600)
            print(barplot_rake_5())
            dev.off()
            write.csv(word_table_rake_5(), f3_rake, row.names = FALSE)
            files_to_zip <- c(f1_rake, f2_rake, f3_rake)
            valid_files <- TRUE
          }
        } 
        else if (input$radio == 1) {
          if (!is.null(wordcloud_data_tf_5()) && 
              !is.null(barplot_tf_5()) && 
              !is.null(word_table_tf_5())) {
            f1_tf <- "wordcloud_tf.png"
            f2_tf <- "barPlot_tf.png"
            f3_tf <- "table_tf.csv"
            png(f1_tf, width = 800, height = 600)
            print(wordcloud(wordcloud_data_tf_1()[["word"]], wordcloud_data_tf_1()[["tf"]], min.freq = 0))
            dev.off()
            png(f2_tf, width = 800, height = 600)
            print(barplot_tf_5())
            dev.off()
            write.csv(word_table_tf_5(), f3_tf, row.names = FALSE)
            files_to_zip <- c(f1_tf, f2_tf, f3_tf)
            valid_files <- TRUE
          }
        }
        
        if (valid_files && length(files_to_zip) > 0) {
          existing_files <- files_to_zip[file.exists(files_to_zip)]
          if (length(existing_files) > 0) {
            zip::zip(zipfile = file, files = existing_files)
          } else {
            stop("Файлы для архивации не были созданы")
          }
        } else {
          stop("Нет данных для экспорта. Сначала создайте таблицу и графики.")
        }
      }, error = function(e) {
        stop(sprintf("Ошибка при создании архива: %s", e$message))
      })
    }
  )
  output$downloadDataCompare <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".zip", sep="")
    },
    content = function(file) {
      tmpdir <- tempdir()
      oldwd <- setwd(tmpdir)
      on.exit(setwd(oldwd))
      files_to_zip <- character(0)
      valid_files <- FALSE
      tryCatch({
        if (input$radio == 2) {
          if (!is.null(cos_mat_reactive_rake()) && 
              !is.null(trend_map_reactive_rake())) {
            f1_rake <- "cos_matrix_rake.csv"
            f2_rake <- "trend_map_rake.png"
            write.csv(cos_mat_reactive_rake(), f1_rake, row.names = FALSE, col.names = FALSE)
            png(f2_rake, width = 800, height = 600)
            print(trend_map_reactive_rake())
            dev.off()
            files_to_zip <- c(f1_rake, f2_rake)
            valid_files <- TRUE
          }
        } 
        else if (input$radio == 1) {
          if (!is.null(cos_mat_reactive_tf()) && 
              !is.null(trend_map_reactive_tf())) {
            f1_tf <- "cos_matrix_tf_idf.csv"
            f2_tf <- "trend_map_tf_idf.png"
            write.csv(cos_mat_reactive_tf(), f1_tf, row.names = FALSE, col.names = FALSE)
            png(f2_tf, width = 800, height = 600)
            print(trend_map_reactive_tf())
            dev.off()
            files_to_zip <- c(f1_tf, f2_tf)
            valid_files <- TRUE
          }
        }
        
        if (valid_files && length(files_to_zip) > 0) {
          existing_files <- files_to_zip[file.exists(files_to_zip)]
          if (length(existing_files) > 0) {
            zip::zip(zipfile = file, files = existing_files)
          } else {
            stop("Файлы для архивации не были созданы")
          }
        } else {
          stop("Нет данных для экспорта. Сначала создайте таблицу и графики.")
        }
      }, error = function(e) {
        stop(sprintf("Ошибка при создании архива: %s", e$message))
      })
    }
  )
  
  observeEvent(input$analyze1, {
    if (input$radio == 1) 
    {
      files_preprocessed_data_frequency[["df_1"]] <- AnalyzeAndRenderFrequency(input[["file1"]], "barPlot1", "wordTable1", "wordcloud1")
    }
    else if (input$radio == 2)
    {
      files_preprocessed_data_rake[["df_1"]] <- AnalyzeAndRenderRake(input[["file1"]], "barPlot1", "wordTable1", "wordcloud1")
    }
    
  })
  observeEvent(input$analyze2, {
    if (input$radio == 1) 
    {
      files_preprocessed_data_frequency[["df_2"]] <- AnalyzeAndRenderFrequency(input[["file2"]], "barPlot2", "wordTable2", "wordcloud2")
    }
    else if (input$radio == 2)
    {
      files_preprocessed_data_rake[["df_2"]] <- AnalyzeAndRenderRake(input[["file2"]], "barPlot2", "wordTable2", "wordcloud2")
    }
  })
  observeEvent(input$analyze3, {
    if (input$radio == 1) 
    {
      files_preprocessed_data_frequency[["df_3"]] <- AnalyzeAndRenderFrequency(input[["file3"]], "barPlot3", "wordTable3", "wordcloud3")
    }
    if (input$radio == 2)
    {
      files_preprocessed_data_rake[["df_3"]] <- AnalyzeAndRenderRake(input[["file3"]], "barPlot3", "wordTable3", "wordcloud3")
    }
  })
  observeEvent(input$analyze4, {
    if (input$radio == 1) 
    {
      files_preprocessed_data_frequency[["df_4"]] <- AnalyzeAndRenderFrequency(input[["file4"]], "barPlot4", "wordTable4", "wordcloud4")
    }
    if (input$radio == 2)
    {
      files_preprocessed_data_rake[["df_4"]] <- AnalyzeAndRenderRake(input[["file4"]], "barPlot4", "wordTable4", "wordcloud4")
    }
  })
  observeEvent(input$analyze5, {
    if (input$radio == 1) 
    {
      files_preprocessed_data_frequency[["df_5"]] <- AnalyzeAndRenderFrequency(input[["file5"]], "barPlot5", "wordTable5", "wordcloud5")
    }
    if (input$radio == 2)
    {
      files_preprocessed_data_rake[["df_5"]] <- AnalyzeAndRenderRake(input[["file5"]], "barPlot5", "wordTable5", "wordcloud5")
    }
  })
  observeEvent(input[["compareFilesBtn"]], {
    if (input$radio == 1) 
    {
      ObserveEventCompareFilesBtnFrequency()
    }
    if (input$radio == 2)
    {
      ObserveEventCompareFilesBtnRake()
    }
  })
}

shinyApp(ui = ui, server = server)
