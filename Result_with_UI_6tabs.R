InstallOrLoadPacks <- function(packs){
  create.pkg <- packs[!(packs %in% installed.packages()[, "Package"])]
  if (length(create.pkg))
    install.packages(create.pkg, dependencies = TRUE)
  sapply(packs, library, character.only = TRUE)
}


packages <- c("ggrepel", "ggplot2",  "data.table", "tm", "wordcloud2", "tidytext", 
              "dplyr", 'tidyverse', 'readxl', 'udpipe', 'writexl', 'openxlsx', 'rlang', 
              'lsa', 'shiny', 'wordcloud')
InstallOrLoadPacks(packages)


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


GetStopwords <- function() {
  # V1 - имя первого столбца по умолчанию
  female_names_rus <- tolower(read.csv("female_names_rus.txt", header=FALSE)$V1)
  male_names_rus <- tolower(read.csv("male_names_rus.txt", header=FALSE)$V1)
  male_surnames_rus <- tolower(read.csv("male_surnames_rus.txt", header=FALSE)$V1)
  STOPWORDS_EXPANDED <- c('и', 'й', 'г', 'единаяроссия', 'единый', 'время', 
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
                           'итог', 'татарстан', 'татарстать', 'российский', 'ма', 'область', 'республика', 'саратовский', 'татарстан', 'татарстана', 'самарский','экономический', 'экономика', 'регион', 'год', "миннихан", "рт", "россия", "рустам", "руст", 'россия', 'конкурентоспособность', 'инновация', 'инвестиция', 'инвестиционный', 'рустамминнихан', 'дмитрий', 'азаров', 'саратовскаяобласть', 'саратовская', 'самарскаяобласть', 'азар', 'стать', 'rn«', 'твой', 'сих', 'ком', 'свой',
                           'слишком', 'нами', 'всему', 'будь', 'саму', 'чаще', 'ваше', 'наш', 'затем', 'еще', 'наши', 'ту', 'каждый',
                           'мочь', 'весь', 'этим', 'наша', 'своих', 'оба', 'который', 'зато', 'те', 'вся', 'ваш', 'такая', 'теми', 'ею', 'нередко',
                           'также', 'чему', 'собой', 'нем', 'вами', 'ими', 'откуда', 'такие', 'тому', 'та', 'очень', 'нему',  'д',
                           'алло', 'оно', 'кому', 'тобой', 'таки', 'мой', 'нею', 'ваши', 'ваша', 'кем', 'мои',
                           'однако', 'сразу', 'свое', 'ними', 'всё', 'неё', 'тех', 'хотя', 'всем', 'тобою', 'тебе', 'одной', 'другие',
                           'буду', 'моё', 'своей', 'такое', 'всею', 'будут', 'своего', 'кого', 'свои', 'мог', 'нам', 'особенно', 'её',
                           'наше', 'кроме', 'вообще', 'вон', 'мною', 'никто', 'это', 'изза', 'именно', 'поэтому', 'будьт', 'являться', 
                           'чувашский', 'тыса', 'смочь', 'ваший', 'гльба', 'ать', 'уть', 'ивать', 'ольги', 'пенз', 'ер', 'иметь', 'олегнуть', 
                           'сг', 'например', 'сообщить', 'сообщать', 'среди', 'нть', 'пер', 'зспермь', 'края', 'ради', 'назвать', 'важный',
                           'ик', 'ульяновсок', 'ульяновска', 'russia', 'reg', 'видео', 'russianpolice', 'mvd', 'police', 'ульяновскаяобласть', 'ульяновскаобласть',
                           'русский', 'личный',
                           
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
                           'мбоу', 'сош', 'айдадомой', 'выступить', 'наштатарстать',
                           'раисрт'
  
                           
  )
  
  STOPWORDS_SHORT <- c('и', 'й', 'г', 'единаяроссия', 'единый', 'время', 'территория', 'димитровграда', 'димитровград', 
                        'чебоксар', 'ядринский', 'житель', 'компания', 'министр', 'дом', 'общественный',
                        'программа', 'мероприятие', 'условие', 'ситуация', 'гражданин', 'группа', 'организация',
                        'система', 'оао', 'ооо', 'пао', 'зао', 'центр', 'руководитель', 'конкурс', 'решить', 'говорить',
                        'состав', 'уровень', 'адрес', 'дело', 'заместитель', 'просто', 'данный', 'сайт', 'позолять',
                        'директор', 'полный', 'час', 'неделя', 'рука', 'ядрин', 'погода', 'выбрать',
                        'средство', 'принять', 'чебоксарский', 'пункт', 'получать', 'второй', 'количество',
                        'нижегородец', 'энгельсский', 'саратовец', 'саратовый', 'балашовский', 'саратов', 'сарат',
                        'градус', 'ребенок', 'ребёнок', 'ый', 'находится', 'находиться', 'сильный', 'перй', 'ул', 
                        'прогнозируть', 'красноярский', 'самар', 'толльятъ', 'тольять', 'толльять','тольятть', 'ярин',
                        
                        
                        'найти', 'pgrunews', 'хороший', 'занятие', 'ныжный', 'номер', 'отлично', 'лс',
                        'состояние', 'цена', 'вид', 'руба', 'зеркадать', 'ть', 'торг', 'нужный', 'муниципальный',
                        'округ', 'ип', 'ссылка', 'задний', 'друг', 'замить', 'бесплатный', 
                        'ульяновскаяобласть', 'ульяновск', 'ульяновский', 'саранск', 'саранска', 'мордовие', 'рм', 'рма', 'мордовия', 
                        'мордовский', 'заец', 'idюрий', 'главамарийэл', 'марий', 'эл', 'марийэл', 'эть', 'васил', 'чурин', 'кировский', 
                        'кировскаяобласть', 'вятский', 'мельниченко', 'месяц', 'оренбургнуть', 'объясняемрф', 'провести', 'инвестор', 
                        'вести', 'реализация', 'башкортостанный', 'радий', 'подписать', 'проект', 'пермский', 'пермскийкрай', 'край', 
                        'прикамья', 'краевой', 'задача', 'важно', 'оренбуржец', 'оренбург', 'новость', 'подчеркнуть', 'оренбуржье', 
                        'оренбургский', 'оренбургскаяобласть', 'поддержка', 'часть', 'км', 'валерийрадаеть', 'олегнуть', 'должен', 
                        'около', 'рассказать', 'глава', 'губернатор', 'развитие', 'январь', 'февраль', 'март', 'апрель', 'май', 'июнь', 
                        'июль', 'август', 'сентябрь', 'октябрь', 'ноябрь', 'декабрь', 'город', 'ecom', 'казань', 'подробность', 'подробный', 
                        'радия', 'процент', 'уф', 'часть', 'вопрос', 'делать', 'сделать', 'благодаря', 'участие', 'пройти', 'идти', 'создать', 
                        'создавать', 'дать',  'рамка', 'место', 'получить', 'удмуртия', 'радай', 'юлие', 'пензенский', 'пенза', 
                        'пензенскаяобласть', 'новый', 'лучший', 'самый', 'работа', 'рабочий', 'работать', 'региональный', 'нижегородскаяобладать', 
                        'clubнижегородский', 'нижегородскаяобласть', 'нижегородский', 'нижний', 'новгород', 'чувашия', 'чувашие', 'обть', "бaшҡортостать", 
                        "бaшҡортостан", 'командахабиров', 'рб', 'миллиард', 'башкирия', 'башкортостан', 'башкортостана', 'мый', 'аный', 'мухаметшина', 
                        'мухаметшин', 'реть', 'рф', 'день', 'отметить', 'число', 'миллион', 'ход', 'президент','страна', 'тысяча', 'рубль', 'доллар', 
                        'район', 'итог', 'татарстан', 'татарстать', 'российский', 'ма', 'область', 'республика', 'саратовский', 'татарстан', 'татарстана', 
                        'самарский','экономический', 'экономика', 'регион', 'год', "миннихан", "рт", "россия", "рустам", "руст", 'россия', 'конкурентоспособность', 
                        'инновация', 'инвестиция', 'инвестиционный', 'рустамминнихан', 'дмитрий', 'азаров', 'саратовскаяобласть', 'саратовская', 'самарскаяобласть', 
                        'азар', 'стать', 'rn«', 'твой', 'сих', 'ком', 'свой',
                        'слишком', 'нами', 'всему', 'будь', 'саму', 'чаще', 'ваше', 'наш', 'затем', 'еще', 'наши', 'ту', 'каждый',
                        'мочь', 'весь', 'этим', 'наша', 'своих', 'оба', 'который', 'зато', 'те', 'вся', 'ваш', 'такая', 'теми', 'ею', 'нередко',
                        'также', 'чему', 'собой', 'нем', 'вами', 'ими', 'откуда', 'такие', 'тому', 'та', 'очень', 'нему',  'д',
                        'алло', 'оно', 'кому', 'тобой', 'таки', 'мой', 'нею', 'ваши', 'ваша', 'кем', 'мои',
                        'однако', 'сразу', 'свое', 'ними', 'всё', 'неё', 'тех', 'хотя', 'всем', 'тобою', 'тебе', 'одной', 'другие',
                        'буду', 'моё', 'своей', 'такое', 'всею', 'будут', 'своего', 'кого', 'свои', 'мог', 'нам', 'особенно', 'её',
                        'наше', 'кроме', 'вообще', 'вон', 'мною', 'никто', 'это', 'изза', 'именно', 'поэтому', 'будьт', 'являться', 
                       'чувашский', 'тыса', 'смочь', 'ваший', 'гльба', 'ать', 'уть', 'ивать', 'ольги', 'пенз', 'ер', 'иметь', 'олегнуть', 
                       'сг', 'например', 'сообщить', 'сообщать', 'среди', 'нть', 'пер', 'зспермь', 'края', 'ради', 'назвать', 'важный',
                       'наштатарстать', 'раисрт')
  # в качестве extra_stopwords можно выбрать или STOPWORDS_EXPANDED, или STOPWORDS_SHORT
  extra_stopwords <- STOPWORDS_EXPANDED
  result_stopwords <- c(stopwords("russian"), extra_stopwords,
                               male_names_rus,
                               male_surnames_rus,
                               female_names_rus)
  return(result_stopwords)
}


stopwords_combined_list <- GetStopwords()
stopwords_combined_str <- paste(stopwords_combined_list, collapse = "|")
PUNCTUATION_MARKS <- c('.', ',', ';', ':', '!', '?', '-', '"', '(',
                                  ')', '«', '»')

LABEL_CHOOSE_RADIO_BUTTON <- "Метод извлечения ключевых слов:"
LABEL_ACTION_OUTPUT_RESULTS <- "Вывод результатов"
LABEL_ACTION_DOWNLOAD_RESULTS <- "Скачать результаты вычислений"
LABEL_CHOOSE_FILE <- "Выбор файла"
LABEL_FILE_INPUT_BUTTON <- "Открыть..."
LABEL_FILE_INPUT_PLACEHOLDER <- "Файл не выбран"
LABEL_CALCULATIONS_IN_PROGRESS <- "Ведутся вычисления."
LABEL_CALCULATIONS_COMPLETED <- "Вычисления окончены."
WIDTH_OF_SIDEBAR_PANEL <- 5
WIDTH_OF_MAIN_PANEL <- 7
TIME_OF_NOTIFICATION_DURATION <- 20
MAX_REQUEST_SIZE <- 30*1024^2 # Почему такой?
SPECIAL_MARKS <- paste0(
  "[",
  "«№",                                         # Спецсимволы кавычки и номер
  "\U{1F600}-\U{1F64F}",                        # Эмодзи (смайлики)
  "\U{1F300}-\U{1F5FF}",                        # Эмодзи (символы)
  "\U{1F680}-\U{1F6FF}",                        # Эмодзи (транспорт)
  "\U{1F1E0}-\U{1F1FF}",                        # Флаги стран
  "\U{2500}-\U{2BEF}",                          # Геометрические фигуры и символы
  "\U{2702}-\U{27B0}",                          # Разные символы
  "\U{24C2}-\U{1F251}",                         # Дополнительные эмодзи
  "\U{1f926}-\U{1f937}",                        # Жесты и люди
  "\U{10000}-\U{10ffff}",                       # Дополнительные Unicode символы
  "\u{2640}-\u{2642}",                          # Символы пола
  "\u{2600}-\u{2B55}",                          # Астрологические и разные символы
  "\u{200d}",                                   # Zero-width joiner
  "\u{23cf}",                                   # Часы
  "\u{23e9}",                                   # Кнопки воспроизведения
  "\u{231a}",                                   # Часы
  "\u{fe0f}",                                   # Variation selector
  "\u{3030}",                                   # Волна
  "\U{00B0}",                                   # Знак градуса
  "\U{20BD}",                                   # Знак рубля
  "]"
)
UDPIPE_PARALLEL_CORES = 8L # Определяет количество ядер, используемых для аннотирования
  # с помощью параллельных вычислений. 
  # Увеличение PARALLEL_CORES не ведет к улучшению производительности (пробовал 12L, 16L).
  # При UDPIPE_PARALLEL_CORES = 4L производительность ниже, чем при 8L.


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
    label = LABEL_CHOOSE_RADIO_BUTTON
   , 
    choices = list( 
      "TF" = 1, 
      "RAKE" = 2
    )
  ), 
  tabsetPanel(id = "tabs",
              tabPanel("Период 1",
                       sidebarLayout(
                         sidebarPanel(
                           fileInput("file1", label = LABEL_CHOOSE_FILE, 
                                     buttonLabel = LABEL_FILE_INPUT_BUTTON, 
                                     placeholder = LABEL_FILE_INPUT_PLACEHOLDER,
                                     accept = ".xlsx"),
                           actionButton("analyze1", LABEL_ACTION_OUTPUT_RESULTS),
                           downloadButton("downloadData1", LABEL_ACTION_DOWNLOAD_RESULTS),
                           width = WIDTH_OF_SIDEBAR_PANEL
                         ),
                         mainPanel(
                           wordcloud2Output("wordcloud1"),
                           plotOutput("barPlot1"),
                           #plotOutput("wordcloud1"),
                           tableOutput("wordTable1"),
                           width = WIDTH_OF_MAIN_PANEL
                         )
                       )
              ),
              tabPanel("Период 2",
                       sidebarLayout(
                         sidebarPanel(
                           fileInput("file2", label = LABEL_CHOOSE_FILE, 
                                     buttonLabel = LABEL_FILE_INPUT_BUTTON, 
                                     placeholder = LABEL_FILE_INPUT_PLACEHOLDER,
                                     accept = ".xlsx"),
                           actionButton("analyze2", LABEL_ACTION_OUTPUT_RESULTS),
                           downloadButton("downloadData2", LABEL_ACTION_DOWNLOAD_RESULTS),
                           width = WIDTH_OF_SIDEBAR_PANEL
                         ),
                         mainPanel(
                           wordcloud2Output("wordcloud2"),
                           plotOutput("barPlot2"),
                           tableOutput("wordTable2"),
                           width = WIDTH_OF_MAIN_PANEL
                         )
                       )
              ),
              tabPanel("Период 3",
                       sidebarLayout(
                         sidebarPanel(
                           fileInput("file3", label = LABEL_CHOOSE_FILE, 
                                     buttonLabel = LABEL_FILE_INPUT_BUTTON, 
                                     placeholder = LABEL_FILE_INPUT_PLACEHOLDER,
                                     accept = ".xlsx"),
                           actionButton("analyze3", LABEL_ACTION_OUTPUT_RESULTS),
                           downloadButton("downloadData3", LABEL_ACTION_DOWNLOAD_RESULTS),
                           width = WIDTH_OF_SIDEBAR_PANEL
                         ),
                         mainPanel(
                           wordcloud2Output("wordcloud3"),
                           plotOutput("barPlot3"),
                           tableOutput("wordTable3"), 
                           width = WIDTH_OF_MAIN_PANEL
                         )
                       )
              ),
              tabPanel("Период 4",
                       sidebarLayout(
                         sidebarPanel(
                           fileInput("file4", label = LABEL_CHOOSE_FILE, 
                                     buttonLabel = LABEL_FILE_INPUT_BUTTON, 
                                     placeholder = LABEL_FILE_INPUT_PLACEHOLDER,
                                     accept = ".xlsx"),
                           actionButton("analyze4", LABEL_ACTION_OUTPUT_RESULTS),
                           downloadButton("downloadData4", LABEL_ACTION_DOWNLOAD_RESULTS),
                           width = WIDTH_OF_SIDEBAR_PANEL
                         ),
                         mainPanel(
                           wordcloud2Output("wordcloud4"),
                           plotOutput("barPlot4"),
                           tableOutput("wordTable4"), 
                           width = WIDTH_OF_MAIN_PANEL
                         )
                       )
              ),
              tabPanel("Период 5",
                       sidebarLayout(
                         sidebarPanel(
                           fileInput("file5", label = LABEL_CHOOSE_FILE, 
                                     buttonLabel = LABEL_FILE_INPUT_BUTTON, 
                                     placeholder = LABEL_FILE_INPUT_PLACEHOLDER,
                                     accept = ".xlsx"),
                           actionButton("analyze5", LABEL_ACTION_OUTPUT_RESULTS),
                           downloadButton("downloadData5", LABEL_ACTION_DOWNLOAD_RESULTS),
                           width = WIDTH_OF_SIDEBAR_PANEL
                         ),
                         mainPanel(
                           wordcloud2Output("wordcloud5"),
                           plotOutput("barPlot5"),
                           tableOutput("wordTable5"), 
                           width = WIDTH_OF_MAIN_PANEL
                         )
                       )
              ),
              tabPanel("Период 6",
                       sidebarLayout(
                         sidebarPanel(
                           fileInput("file6", label = LABEL_CHOOSE_FILE, 
                                     buttonLabel = LABEL_FILE_INPUT_BUTTON, 
                                     placeholder = LABEL_FILE_INPUT_PLACEHOLDER,
                                     accept = ".xlsx"),
                           actionButton("analyze6", LABEL_ACTION_OUTPUT_RESULTS),
                           downloadButton("downloadData6", LABEL_ACTION_DOWNLOAD_RESULTS),
                           width = WIDTH_OF_SIDEBAR_PANEL
                         ),
                         mainPanel(
                           wordcloud2Output("wordcloud6"),
                           plotOutput("barPlot6"),
                           tableOutput("wordTable6"), 
                           width = WIDTH_OF_MAIN_PANEL
                         )
                       )
              ),
              tabPanel("Оценка динамики",
                       actionButton("compareFilesBtn", "Сравнить проанализированные файлы"),
                       downloadButton("downloadDataCompare", LABEL_ACTION_DOWNLOAD_RESULTS),
                       tableOutput("compareFilesTable"),
                       plotOutput("trendMapLimited"),
                       # plotOutput("dynamicPlotAll"), # сейчас не используется.
              )
  )
)


server <- function(input, output, session) {
  options(shiny.maxRequestSize=MAX_REQUEST_SIZE)
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
  
  wordcloud_data_tf_6 <- reactiveVal(NULL)
  wordcloud_data_rake_6 <- reactiveVal(NULL)
  barplot_tf_6 <- reactiveVal(NULL)
  barplot_rake_6 <- reactiveVal(NULL)
  word_table_tf_6 <- reactiveVal(NULL)
  word_table_rake_6 <- reactiveVal(NULL)
  
  cos_mat_reactive_tf <- reactiveVal(NULL)
  trend_map_plot_tf <- reactiveVal(NULL)
  cos_mat_reactive_rake <- reactiveVal(NULL)
  trend_map_plot_rake <- reactiveVal(NULL)  
  # для перевода тренд-карт на английский
  trend_map_df_tf <- reactiveVal(NULL)
  trend_map_df_rake <- reactiveVal(NULL)
  
  
  # Удаление лишних пробелов бесполезно, так как их устраняют  при токенизации,
  # Приведение текста к кодировке UTF-8 может быть полезно
  # Удаление цифр полезно
  # Приведение к нижнему регистру полезно, так как оно не происходит 
  # при keywords_rake и udpipe_annotate
  CleanCorpusRake <- function(corpus_to_use){ 
    corpus_to_use %>%
      # tm_map(removePunctuation) %>% # удаление стандартных знаков пунктуации, 
            # знаки пунктуации являются разделителями фраз 
            # при выделении ключевых фраз с помощью RAKE.
            # Поэтому перед использованием RAKE не нужно удалять знаки пунктуации.
      tm_map(stripWhitespace) %>%  
      tm_map(content_transformer(function(x) iconv(x, to='UTF-8'))) %>% # приведение к UTF8
      tm_map(removeNumbers) %>% # удаление цифр
      tm_map(content_transformer(tolower)) # приведение к нижнему регистру
  }
  
  
  # все команды этой функции совпадают с соотв-ми командами алгоритма для 14 регионов
  CleanCorpusFrequency <- function(corpus_to_use){  
    corpus_to_use %>%
      tm_map(removePunctuation) %>%  # удаление стандартных знаков пунктуации, 
      tm_map(stripWhitespace) %>%    
      tm_map(content_transformer(function(x) iconv(x, to='UTF-8'))) %>% # приведение к UTF8
      tm_map(removeNumbers) %>% # удаление цифр
      tm_map(content_transformer(tolower)) # приведение к нижнему регистру
  }
  
  
  GetReplacedText <- function(replacements, text) {
    for (pattern in names(replacements)) {
      text <- gsub(pattern, replacements[[pattern]], text, perl = TRUE)
    }
    return(text)
  }
  
  
  GetRakeKeywords <- function(input_file) {
    # Проверка на корректность ввода файла. 
    # Если файл введен некорректно, то событие (ObserveEvent), 
    # вызввавшее функцию останавливаетя.
    req(input_file)
    showNotification(LABEL_CALCULATIONS_IN_PROGRESS, duration = TIME_OF_NOTIFICATION_DURATION)
    input_data <- as.data.frame(read_excel(input_file$datapath, col_names = FALSE)) 
    
    corp_city_df <- CleanCorpusRake(VCorpus(VectorSource(input_data)))
    # replacements <- list(
    #   SPECIAL_MARKS = "",
    #   "\\b\\S*(http|vk)\\S*\\b" = "",
    #   "движениепервых" = "движение первых",
    #   "навигаторыдетство" = "навигаторы детство",
    #   "новостипервых" = "новости первых",
    #   "фотоомск" = "фото омск",
    #   "большаяучительскаянеделя" = "большая учительская неделя",
    #   "годпедагоганаставник" = "год педагога наставник",
    #   "государственныйсоветреспублики" = "государственный совет республики"
    # )
    # corp_city_df[["1"]][["content"]] <- GetReplacedText(replacements, corp_city_df[["1"]][["content"]])

    corp_city_df[["1"]][["content"]] <- gsub(SPECIAL_MARKS, "", corp_city_df[["1"]][["content"]], perl = TRUE)
    corp_city_df[["1"]][["content"]] <- gsub("\\b\\S*(http|vk)\\S*\\b", "", corp_city_df[["1"]][["content"]], perl = TRUE) # удаление ссылок
    corp_city_df[["1"]][["content"]] <- gsub("движениепервых", "#движение первых#", corp_city_df[["1"]][["content"]], perl = TRUE)
    corp_city_df[["1"]][["content"]] <- gsub("навигаторыдетство", "#навигаторы детство#", corp_city_df[["1"]][["content"]], perl = TRUE)
    corp_city_df[["1"]][["content"]] <- gsub("новостипервых", "#новости первых#", corp_city_df[["1"]][["content"]], perl = TRUE)
    corp_city_df[["1"]][["content"]] <- gsub("фотоомск", "#фото омск#", corp_city_df[["1"]][["content"]], perl = TRUE)
    corp_city_df[["1"]][["content"]] <- gsub("большаяучительскаянеделя", "#большая учительская неделя#", corp_city_df[["1"]][["content"]], perl = TRUE)
    corp_city_df[["1"]][["content"]] <- gsub("годпедагоганаставник", "#год педагога наставник#", corp_city_df[["1"]][["content"]], perl = TRUE)
    corp_city_df[["1"]][["content"]] <- gsub("государственныйсоветреспублики", "#государственный совет республики# ", corp_city_df[["1"]][["content"]], perl = TRUE)
    
    # corp_city_df[["1"]][["content"]] <- str_remove_all(corp_city_df[["1"]][["content"]], "\b\\S*(http|vk)\\S*\b")
    
    # Попробовал использовать russian-gsd-ud-2.15-241121.udpipe.
    # Не вышло скачать. Быть может, ее и для R нет.
    name_of_gsd_model <- 'russian-gsd-ud-2.5-191206.udpipe'
    if (!file.exists(name_of_gsd_model))
    {
      # нужно ли помещать модель в переменную?
      gsd_model_raw <- udpipe_download_model(language = "russian-gsd") 
    }
    gsd_model <- udpipe_load_model(file = name_of_gsd_model)
    
    # IMPORTANT: самая ресурсоемкая строка. Ее выполнение занимает 80-90% времени обработки файла (среза)
    # parallel.cores = 8L сильно увеличивает производительность.
    # Для Башкортастан_посты_2023.xlsx (1,4 Мб) при последовательном исполнении (parallel.cores = 1L)
    # время исполнения udpipe(...) - 1 мин. 38 сек., а общее время - 1 мин. 41 сек.
    # При parallel.cores = 8L время исполнения udpipe(...) - 28 сек., а общее время - 35 сек.
    annotated_texts_df <- udpipe(object = gsd_model, x = corp_city_df[["1"]][["content"]],  
                                 parser = "none", parallel.cores = 1L)
    show("Текст аннотирован")   
    annotated_texts_df <- as.data.frame(annotated_texts_df)
    # до сюда строки повторяют код функции GetPreprocessedTextsWordList

    lemmas <- annotated_texts_df$lemma 
    
    lemmas <- gsub("[[:punct:]]", "", lemmas)
    # for (i in 1:length(tmp))
    # {
    #   show(tmp[i])
    #   # если количество символов > 1
    #   if (nchar(tmp[i]) > 1)
    #   {
    #     # Если термин начинается со знака пунктуации, то удалить 
    #     # все идущие подраяд с начала знаки пунктуации
    #     while(str_sub(tmp[i], 1, 1) %in% basic_punctuation_marks_list)
    #     {
    #       tmp[i] <- str_sub(tmp[i], 2, -1)
    #     }
    #     # Если термин заканчивается знаком пунктуации, то удалить 
    #     # все идущие подраяд с конца знаки пунктуации
    #     show(str_sub(tmp[i], 1, -2))
    #     while(str_sub(tmp[i], -1, -1) %in% basic_punctuation_marks_list)
    #     {
    #       tmp[i] <- str_sub(tmp[i], 1, -2)
    #     }
    #   }
    #   
    # }
    # 
    
    # tmp <- str_replace_all(tmp, '№', '')
    # tmp <- str_replace_all(tmp, '−', '')
    # tmp <- str_replace_all(tmp, '—', '')
    lemmas <- str_replace_all(lemmas, 'правительстворазвитие', 'развитие')
    # tmp <- str_replace_all(tmp, 'правительстворб', 'правительство')
    lemmas <- str_replace_all(lemmas, 'цифровый', 'цифровой')
    lemmas <- str_replace_all(lemmas, 'научныймощность', 'научный мощность')
    lemmas <- str_replace_all(lemmas, 'club', '')
    lemmas <- str_replace_all(lemmas, 'ветр', 'ветер')
    lemmas <- str_replace_all(lemmas, 'школьник', 'школа')
    lemmas <- str_replace_all(lemmas, 'школьный', 'школа')
    # tmp <- str_replace_all(tmp, 'правительствомарийэть', 'правительство')
    lemmas <- str_replace_all(lemmas, 'молние', 'молния')
    
    lemmas <- str_replace_all(lemmas, 'полицияроссия', 'полиция')
    lemmas <- str_replace_all(lemmas, 'осуждеть', 'осуждать')
    lemmas <- str_replace_all(lemmas, 'умвд', 'мвд') 
    lemmas <- str_replace_all(lemmas, 'юнармеец', 'юнармия') 
    lemmas <- str_replace_all(lemmas, 'движениепервый', 'движениепервых') 
    lemmas <- str_replace_all(lemmas, 'перевозкий', 'перевозка') 
    lemmas <- str_replace_all(lemmas, 'юнармейский', 'юнармия') 
    # tmp <- tmp[!grepl("\\b\\w*(http|vk)\\S*\\b", tmp)]  # Удаление терминов, содержащих http или vk
    # tmp <- tmp[sapply(tmp, nchar) > 0]
    
    annotated_texts_df$lemma <- lemmas
    
    
    # Определение функции keywords_rake с некоторыми изменениями
    keywords_rake_test <- function(x, term, group, relevant = rep(TRUE, nrow(annotated_texts_df)), ngram_max = 2, n_min = 30, sep = " ") {
      # Объявление переменных для избежания предупреждений при проверке пакета
      .relevant <- .N <- keyword_id <- keyword <- degree <- word <- freq <- ngram <- rake <- rake_word_score <- NULL

      # Проверка входных данных:
      stopifnot(is.data.frame(x))  # x должен быть data.frame
      stopifnot(term %in% colnames(x))  # term должен быть столбцом в x
      stopifnot(all(group %in% colnames(x)))  # все элементы group должны быть столбцами x
      stopifnot(length(relevant) == nrow(x))  # длина relevant должна совпадать с числом строк x

      # Преобразование группирующих переменных:
      if (length(group) > 1) {
        # Если несколько групп, создаем уникальный идентификатор группы
        x <- data.table(group = unique_identifier(x, fields = group),
                        word = x[[term]], .relevant = relevant)
      } else {
        # Если одна группа, берем ее напрямую
        x <- data.table(group = x[[group]], word = x[[term]], .relevant = relevant)
      }
      # Создание уникального ID для последовательных релевантных сегментов
      x$keyword_id <- data.table::rleid(x[["group"]], x[[".relevant"]])
      
      # Фильтрация: оставляем только релевантные строки и выбираем нужные столбцы
      x <- subset(x, .relevant != FALSE, select = c("keyword_id", "word"))
      
      # Мной вставлено.  # Удалить пустые строки
      x <- subset(x, nzchar(trimws(word)))
      # Формирование ключевых слов: объединение слов по keyword_id с разделителем sep
      x <- x[, `:=`(keyword, paste(word, collapse = sep)), by = list(keyword_id)]

      # Вычисление степени (degree) для каждого слова: количество слов в ключевой фразе - 1
      x <- x[, `:=`(degree, .N - 1L), by = list(keyword_id)]
      # show(length(unlist(x)))

      #show(unlist(x))
      # Подготовка данных для расчета RAKE:
      word_score <- list()
      # Суммирование степени для каждого уникального слова
      word_score$degree <- x[, list(degree = sum(degree)), by = list(word)]
      # Расчет частоты слов с помощью функции txt_freq
      # word_score$freq - таблица, состоящая из столбцов key, freq, freq_pct
      # key - уникальное слово (не фраза), freq - сколько раз оно встречается,
      # freq_pct - отношение частоты слова к количеству всех слов.
      word_score$freq <- txt_freq(x$word)
      word_score$freq <- setDT(word_score$freq)  # Преобразование в data.table


      # Объединение данных о степени и частоте слов
      word_score <- merge(word_score$degree, word_score$freq,
                          by.x = "word", by.y = "key", all.x = FALSE, all.y = TRUE)

      # Расчет RAKE-оценки для каждого слова: степень / частота
      word_score$rake_word_score <- word_score$degree / word_score$freq
      show(x)
      show(word_score)
      # Сбор статистики по ключевым словам:
      # Частота ключевых фраз (по уникальным keyword_id)
      keywords <- x[, list(freq = length(unique(keyword_id))), by = list(keyword, word)]

      # Добавление RAKE-оценок слов к ключевым словам
      keywords <- merge(keywords, word_score[, c("word", "rake_word_score"), with = FALSE],
                        by = "word", all.x = TRUE, all.y = FALSE)
      show(keywords)
      # Агрегация: расчет общего RAKE и ngram для каждого ключевого слова
      # keywords <- keywords[, list(ngram = .N, rake = sum(rake_word_score)),
      #                      by = list(keyword, freq)]
      keywords <- keywords[, list(
        ngram = sapply(strsplit(keyword[1], sep, fixed = TRUE), length),
        rake = sum(rake_word_score)
      ), by = list(keyword, freq)]
      
      
      # я добавили эту строку для отладки
      # setorder(keywords, -rake)
      show(keywords)
      for(keyword in keywords$keyword)
      {
        if (str_detect(keyword, "движение")){
          show(keyword)
        }
      }
      # Фильтрация результатов по максимальному ngram и минимальной частоте
      keywords <- subset(keywords, ngram <= ngram_max & freq >= n_min)
      
      
      
      # Добавлено мной. Список фраз для исключения (регистрозависимый)
      exclude_phrases <- c("первый", 'президентский нацпроект',
       'активист движение', 'почетный гость', 'электронный почта', 
       'приятный поездка', 'малый класс', 'социальный сеть', 
       'юнармия', 'отряд', 'детский', 'поздравление первый')
      
      # Добавлено мной. Фильтрация через %in% с отрицанием
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
    
    
    # Оставлять только существительные и прилагательные. 
    # В качестве терминов берутся слова из таблицы annotated_texts_df из столбца lemma,
    # то есть начальные формы слов.
    # Оставлять только фразы, частота встречаемости которых >= параметра n_min
    # Метод keywords_rake возвращает таблицу со столбцами keyword, ngram, freq, rake;
    # ключевые фразы в таблице отсортированы по убыванию столбца rake. 
    
    min_freq_of_phrase <- 10
    keywords_rake_df <- keywords_rake_test(annotated_texts_df, term = "lemma", group = c("sentence_id"),
                                      relevant = annotated_texts_df$upos %in% c("NOUN", "ADJ") &
                                        !(annotated_texts_df$lemma %in% stopwords_combined_list),
                                      ngram_max = 3, n_min = min_freq_of_phrase)
    show(keywords_rake_df)
    return(keywords_rake_df)
  }
  
  
  # все команды этой функции совпадают с соотв-ми командами алгоритма для 14 регионов
  GetTFIDFKeywords <- function(file) {   
    # Проверка на корректность ввода файла. 
    # Если файл введен некорректно, то событие (ObserveEvent), 
    # вызввавшее функцию останавливаетя.
    req(file)
    showNotification(LABEL_CALCULATIONS_IN_PROGRESS, duration = TIME_OF_NOTIFICATION_DURATION)
    input_data <- as.data.frame(read_excel(file$datapath, col_names = FALSE)) 
    corp_city_df <- CleanCorpusFrequency(VCorpus(VectorSource(input_data)))
    corp_city_df[["1"]][["content"]] <- gsub(SPECIAL_MARKS, "", corp_city_df[["1"]][["content"]], perl = TRUE)
    corp_city_df[["1"]][["content"]] <- gsub("\\b\\S*(http|vk)\\S*\\b", "", corp_city_df[["1"]][["content"]], perl = TRUE)
    corp_city_df[["1"]][["content"]] <- gsub("движениепервых", "движение первых", corp_city_df[["1"]][["content"]], perl = TRUE)
    corp_city_df[["1"]][["content"]] <- gsub("навигаторыдетство", "навигаторы детство", corp_city_df[["1"]][["content"]], perl = TRUE)
    corp_city_df[["1"]][["content"]] <- gsub("новостипервых", "новости первых", corp_city_df[["1"]][["content"]], perl = TRUE)
    corp_city_df[["1"]][["content"]] <- gsub("фотоомск", "фото омск", corp_city_df[["1"]][["content"]], perl = TRUE)
    corp_city_df[["1"]][["content"]] <- gsub("большаяучительскаянеделя", "большая учительская неделя", corp_city_df[["1"]][["content"]], perl = TRUE)
    corp_city_df[["1"]][["content"]] <- gsub("годпедагоганаставник", "год педагога наставник", corp_city_df[["1"]][["content"]], perl = TRUE)
    corp_city_df[["1"]][["content"]] <- gsub("государственныйсоветреспублики", "государственный совет республики ", corp_city_df[["1"]][["content"]], perl = TRUE)
    
    
    # replacements <- list(
    #   SPECIAL_MARKS = "",
    #   "\\b\\S*(http|vk)\\S*\\b" = "",
    #   "движениепервых" = "движение первых",
    #   "навигаторыдетство" = "навигаторы детство",
    #   "новостипервых" = "новости первых",
    #   "фотоомск" = "фото омск",
    #   "большаяучительскаянеделя" = "большая учительская неделя",
    #   "годпедагоганаставник" = "год педагога наставник",
    #   "государственныйсоветреспублики" = "государственный совет республики"
    # )
    # text <- corp_city_df[["1"]][["content"]]
    # for (pattern in names(replacements)) {
    #   text <- gsub(pattern, replacements[[pattern]], text, perl = TRUE)
    # }
    # corp_city_df[["1"]][["content"]] <- text
    
    
    
    if (!file.exists('russian-gsd-ud-2.5-191206.udpipe'))
    {
      # нужно ли помещать модель в переменную?
      gsd_model_raw <- udpipe_download_model(language = "russian-gsd")
    }
    gsd_model <- udpipe_load_model(file = 'russian-gsd-ud-2.5-191206.udpipe')
    # IMPORTANT: самая ресурсоемкая строка. Ее выполнение занимает 60-70% времени обработки файла (среза)
    # parallel.cores = 8L сильно увеличивает производительность.
    # Для Башкортастан_посты_2023.xlsx (1,4 Мб) при последовательном исполнении (parallel.cores = 1L)
    # время исполнения udpipe(...) - 1 мин. 39 сек., а общее время - 2 мин. 36 сек.
    # При parallel.cores = 8L время исполнения udpipe(...) - 37 сек., а общее время - 1 мин. 33 сек.
    annotated_texts_df <- udpipe(object = gsd_model, x = corp_city_df[["1"]][["content"]],  
                                 parser = "none", parallel.cores = UDPIPE_PARALLEL_CORES)
    show("Текст аннотирован")   
    annotated_texts_df <- as.data.frame(annotated_texts_df)
    
    # TODO: Что значит этот комментарий здесь: "Эта часть кода не выполняет полезной работы сейчас"?
    annotated_texts_df$lemma <- noquote(annotated_texts_df$lemma)
    annotated_texts_df$lemma <- str_replace_all(annotated_texts_df$lemma, "[[:punct:]]", "")
    lemmas <- annotated_texts_df$lemma
    lemmas <- str_replace_all(annotated_texts_df$lemma, paste("\\b(", stopwords_combined_str, ")\\b"), "")
    lemmas <- str_replace_all(lemmas, '№', '')
    lemmas <- str_replace_all(lemmas, '−', '')
    lemmas <- str_replace_all(lemmas, '—', '')
    lemmas <- str_replace_all(lemmas, 'правительстворазвитие', 'развитие')
    # tmp <- str_replace_all(tmp, 'правительстворб', 'правительство')
    lemmas <- str_replace_all(lemmas, 'цифровый', 'цифровой')
    lemmas <- str_replace_all(lemmas, 'научныймощность', 'научный мощность')
    lemmas <- str_replace_all(lemmas, 'club', '')
    lemmas <- str_replace_all(lemmas, 'ветр', 'ветер')
    lemmas <- str_replace_all(lemmas, 'школьник', 'школа')
    lemmas <- str_replace_all(lemmas, 'школьный', 'школа')
    # tmp <- str_replace_all(tmp, 'правительствомарийэть', 'правительство')
    lemmas <- str_replace_all(lemmas, 'молние', 'молния')
    # tmp <- str_replace_all(x$lemma, paste("\\b(", stopwords_combined_str, ")\\b"), "") # без этого остается часто повторяющееся слово "правительство"
    
    lemmas <- str_replace_all(lemmas, 'полицияроссия', 'полиция')
    lemmas <- str_replace_all(lemmas, 'осуждеть', 'осуждать')
    lemmas <- str_replace_all(lemmas, 'умвд', 'мвд') 
    lemmas <- str_replace_all(lemmas, 'юнармеец', 'юнармия') 
    lemmas <- str_replace_all(lemmas, 'движениепервый', 'движениепервых')
    lemmas <- str_replace_all(lemmas, 'перевозкий', 'перевозка') 
    lemmas <- str_replace_all(lemmas, 'юнармейский', 'юнармия') 
    
    lemmas <- str_replace_all(lemmas, 'первый', '') # этим отличается от rake
    lemmas <- lemmas[sapply(lemmas, nchar) > 0]
    return(lemmas)
  }
  
  
  WORDS_COUNT_IN_BARPLOT <- 10
  WORDS_COUNT_IN_TABLE <- 10
  WORDS_COUNT_IN_WORDCLOUD <- 30
  
  
  # ????все команды этой функции совпадают с соотв-ми командами алгоритма для 14 регионов
  AnalyzeAndRenderFrequency <- function(file_input, id_barplot_output, id_table_output, id_wordcloud_output) {   
    preprocessed_texts_word_list <- GetTFIDFKeywords(file_input)
    d <- as.data.frame(sort(table(preprocessed_texts_word_list), decreasing = TRUE))
    # show(d)
    colnames(d) <- c("word", "freq")
    word_freq <- d
    all_freq_for_document <- sum(d$freq)
    # show(all_freq_for_document)
    d$tf <- d$freq / all_freq_for_document
    d_word_tf <- select(d, c("word", "tf"))
    
    
    output[[id_barplot_output]]  <- renderPlot({
      barplot_tf <- ggplot(d_word_tf[1:WORDS_COUNT_IN_BARPLOT, ], 
                           aes(x = reorder(word, tf), y = tf)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        labs(title = "Ключевые слова", x = "Слова", y = "Term Frequency") +
        # labs(title = "Наиболее часто встречающиеся слова", x = "Слова", y = "Частота встречаемости") +
        theme_gray(base_size = 26)
      # show(paste0(file_input, " TF Barplot.png"))
      name_of_input_file <- file_input$name
      
      if (id_barplot_output == "barPlot1")
        barplot_tf_1(barplot_tf)
      else if (id_barplot_output == "barPlot2")
        barplot_tf_2(barplot_tf)
      else if (id_barplot_output == "barPlot3")
        barplot_tf_3(barplot_tf)
      else if (id_barplot_output == "barPlot4")
        barplot_tf_4(barplot_tf)
      else if (id_barplot_output == "barPlot5")
        barplot_tf_5(barplot_tf)
      else if (id_barplot_output == "barPlot6")
        barplot_tf_6(barplot_tf)
      
      
      # barplot_tf_1(barplot_tf)
      # ggsave(paste(name_of_input_file, "TF Barplot.png"), plot = barplots_tf, width = 8, height = 6, dpi = 300)
      barplot_tf
    })
    
    
    output[[id_table_output]] <- renderTable({
      # colnames(word_freq) <- c("Слово", "Частота встречаемости слова в корпусе текстов")
      colnames(d_word_tf) <- c("Слово", "TF")
      table_rake_for_output <- head(d_word_tf, WORDS_COUNT_IN_TABLE)
      
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
      else if (id_table_output == "wordTable6")
        word_table_tf_6(table_rake_for_output)
      
      table_rake_for_output
    }, digits = 4
    )
    # output[[id_wordcloud_output]] <- renderPlot({
    #     wordcloud(d$word, d$freq, colors=brewer.pal(8, "Dark2"))
    #   })
    output[[id_wordcloud_output]] <- renderWordcloud2({
      wordcloud_tf_for_output <- head(d_word_tf, WORDS_COUNT_IN_WORDCLOUD
      )
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
      else if (id_wordcloud_output == "wordcloud6")
        wordcloud_data_tf_6(wordcloud_tf_for_output)
      
      Wordcloud2a(head(d_word_tf, WORDS_COUNT_IN_WORDCLOUD
      ), size = 0.45)
    })
    showNotification(LABEL_CALCULATIONS_COMPLETED, duration = TIME_OF_NOTIFICATION_DURATION)
    show(d)
    return(d)
  }
  
  
  AnalyzeAndRenderRake <- function(file_input, id_barplot_output, id_table_output, id_wordcloud_output) { 
    keywords_rake_df <- GetRakeKeywords(file_input)
    # show(keywords_rake_df)
    keywords_rake_df_for_output <- keywords_rake_df[c("keyword", "freq", "rake")]
    # show(keywords_rake_df_for_output)
    name_of_input_file <- file_input$name
    output[[id_barplot_output]] <- renderPlot({
      diagram_for_output <- ggplot(keywords_rake_df_for_output[1:WORDS_COUNT_IN_BARPLOT, ], 
                                   aes(x = reorder(keyword, rake), y = rake)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        labs(title = "Слова с наибольшим индексом RAKE", x = "Слова", y = "Индекс RAKE") +
        theme_gray(base_size = 26)
      # # wordclouds_rake[[id_wordcloud_output]](diagram_for_output)
      # # ggsave(paste(name_of_input_file, "RAKE Barplot.png"), plot = diagram_for_output, width = 15, height = 6, dpi = 300)
      
      # barplots_rake[[id_barplot_output]] <- keywords_rake_df_for_output[1:10, ]
      if (id_barplot_output == "barPlot1")
        barplot_rake_1(diagram_for_output)
      else if (id_barplot_output == "barPlot2")
        barplot_rake_2(diagram_for_output)
      else if (id_barplot_output == "barPlot3")
        barplot_rake_3(diagram_for_output)
      else if (id_barplot_output == "barPlot4")
        barplot_rake_4(diagram_for_output)
      else if (id_barplot_output == "barPlot5")
        barplot_rake_5(diagram_for_output)
      else if (id_barplot_output == "barPlot6")
        barplot_rake_6(diagram_for_output)
      
      diagram_for_output
      # ggplot(keywords_rake_df_for_output[1:10, ], aes(x = reorder(keyword, rake), y = rake)) +
      #   geom_bar(stat = "identity") +
      #   coord_flip() +
      #   labs(title = "Ключевые слова", x = "Слова", y = "Индекс RAKE") +
      #   theme_gray(base_size = 26)
    })
    output[[id_table_output]] <- renderTable({
      colnames(keywords_rake_df_for_output) <- c("Ключевые слова", "Частота встречаемости", "RAKE")
      table_rake_for_output <- head(keywords_rake_df_for_output, WORDS_COUNT_IN_TABLE)
      #ggsave(filename = paste(name_of_input_file, "RAKE Table.png"), plot = table_rake_for_output, width = 8, height = 6, dpi = 300)
      # table_rake_for_output
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
      else if (id_table_output == "wordTable6")
        word_table_rake_6(table_rake_for_output)
      table_rake_for_output
    })
    keywords_rake_df_for_output_wordcloud <- keywords_rake_df_for_output[c("keyword", "freq")]
    # Сортировка по столбцу freq по убыванию для облака слов
    keywords_rake_df_for_output_wordcloud <- keywords_rake_df_for_output_wordcloud[
      order(keywords_rake_df_for_output_wordcloud$freq, decreasing = TRUE),]
    output[[id_wordcloud_output]] <- renderWordcloud2({
      # wordcloud_rake_for_output <- Wordcloud2a(keywords_rake_df_for_output_wordcloud, size = 0.45)
      # ggsave(paste(name_of_input_file, "RAKE Wordcloud.png"), plot = wordcloud_rake_for_output, width = 8, height = 6, dpi = 300)
      # wordcloud_rake_for_output
      # keywords_rake_df_for_output_wordcloud_top <- head(keywords_rake_df_for_output_wordcloud, amount_of_words_in_wordcloud)
      wordcloud_rake_for_output <- head(keywords_rake_df_for_output_wordcloud, 
                                        WORDS_COUNT_IN_WORDCLOUD)
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
      else if (id_wordcloud_output == "wordcloud6")
        wordcloud_data_rake_6(wordcloud_rake_for_output) 
      # wordclouds_data_rake[[id_wordcloud_output]] <- 
      #   keywords_rake_df_for_output_wordcloud[1:amount_of_words_in_wordcloud, ]
      Wordcloud2a(wordcloud_rake_for_output, size = 0.45)
    })
    showNotification(LABEL_CALCULATIONS_COMPLETED, duration = TIME_OF_NOTIFICATION_DURATION)
    return(keywords_rake_df)
  }

  
  WORDS_COUNT_IN_TRENDMAP <- 10
  LABEL_NEED_MORE_PROCESSED_FILES <- "Для анализа должно быть обработано не менее двух файлов с помощью одного метода."
  
    
  ObserveEventCompareFilesBtnFrequency <- function(){
    d_all <- Filter(Negate(is.null), list(files_preprocessed_data_frequency[["df_1"]], 
                                          files_preprocessed_data_frequency[["df_2"]], 
                                          files_preprocessed_data_frequency[["df_3"]],
                                          files_preprocessed_data_frequency[["df_4"]],
                                          files_preprocessed_data_frequency[["df_5"]],
                                          files_preprocessed_data_frequency[["df_6"]])) 
    cos.mat <- NULL
    if (length(d_all) <= 1) {
      showNotification(LABEL_NEED_MORE_PROCESSED_FILES, 
                       duration = TIME_OF_NOTIFICATION_DURATION)
    }
    else 
    {  
      showNotification(LABEL_CALCULATIONS_IN_PROGRESS, duration = TIME_OF_NOTIFICATION_DURATION)
      amount_of_processed_files <- length(d_all)
      # d_all[[i]] содержит таблицу со столбцами word freq tf
      # Чтобы full_join происходил корректно, 
      # и чтобы не нужно было переименовывать после него столбцы,
      # нужно, чтобы все имена столбцов, кроме столбца, 
      # по которому происходит соединение, были разными. 
      # В данном случае для таблицы с номером i столбцы будут следующими:
      # word freq<i> tf<i>.
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
      # формирование имен столбцов col_names_word_freq_tf, col_names_word_freq
      # и col_names_freq
      # именами freq<i>tf<i>, freq<i>, freq<i>соответственно
      col_names_word_freq_tf <- c('word')
      col_names_word_freq <- c('word')
      col_names_freq <- c()
      for (i in 1:amount_of_processed_files)
      {
        col_names_word_freq_tf <- c(col_names_word_freq_tf,  paste("freq", i, sep = ""), 
                                    paste("tf", i, sep = ""))
        col_names_word_freq <- c(col_names_word_freq, paste("freq", i, sep = ""))
        col_names_freq <- c(col_names_freq, paste("freq", i, sep = ""))
      }
      # tf_idf <- select(d_all, 'word', 'freq1', 'tf1')
      tf_idf <- d_all %>% select(all_of(col_names_word_freq_tf))
      tdm_df <- d_all %>% select(all_of(col_names_word_freq))
      show(tdm_df)
      tdm_df <- tdm_df %>% mutate(num_of_occurrences = 
                                    rowSums(select(tdm_df, all_of(col_names_freq)) != 0))
      # tdm_df$num_of_occurrences - количество файлов, 
      # в которых встречается слово из столбца word
      tdm_df <- tdm_df %>% mutate(idf = log((amount_of_processed_files + 1) / 
                                              (1 + num_of_occurrences) + 1))

      tdm_df_with_dynamism <- tdm_df

      tdm_df_with_dynamism$freq_all <- tdm_df_with_dynamism[['freq1']]
      for (i in 2:amount_of_processed_files)
      {
        tdm_df_with_dynamism$freq_all <- tdm_df_with_dynamism$freq_all +
          tdm_df_with_dynamism[[paste("freq", i, sep = "")]]
      }
      # ???
      # tdm_df_with_dynamism$dynamism <- (tdm_df_with_dynamism$freq2 - tdm_df_with_dynamism$freq1) / ifelse(tdm_df_with_dynamism$freq1 != 0, tdm_df_with_dynamism$freq1, 1)

      # Средний абсолютный прирост

      # Из частоты последнего периода вычитается частота первого периода
      tdm_df_with_dynamism$dynamism <- 
        tdm_df_with_dynamism[[paste("freq", amount_of_processed_files, sep = "")]] -
        tdm_df_with_dynamism[["freq1"]]
      # И результат делится на количество периодов - 1
      tdm_df_with_dynamism$dynamism <- tdm_df_with_dynamism$dynamism / 
        (amount_of_processed_files - 1)
      # Средний коэффициент роста (Средний темп роста)
      # tdm_df_with_dynamism$dynamism <- sqrt((tdm_df_with_dynamism$freq3 / ifelse(tdm_df_with_dynamism$freq1 != 0, tdm_df_with_dynamism$freq1, 1)))

      tf_idf <- tf_idf %>% mutate(num_of_occurrences = tdm_df$num_of_occurrences)
      tf_idf <- tf_idf %>% mutate(idf = tdm_df$idf)
      # .data[[paste("tf_idf", amount_of_processed_files, sep = "")]]
      
      col_names_tf <- c()
      col_names_tfidf <- c()
      for (i in 1:amount_of_processed_files)
      {
        # формирование имен столбцов col_names_tf и col_names_tfidf
        # именами tf<i> и tf_idf<i> соответственно
        col_name_tf <- paste0("tf", i, sep = "")
        col_names_tf <- c(col_names_tf, col_name_tf)
        col_name_tfidf <- paste0("tf_idf", i, sep = "")
        col_names_tfidf <- c(col_names_tfidf, col_name_tfidf)
        
        tf_idf[[col_name_tfidf]] <- tf_idf[[col_name_tf]] * tf_idf$idf
      }

      tf_idf_only <- select(tf_idf, all_of(col_names_tfidf))

      period_names <- c()
      for (i in 1:amount_of_processed_files)
      {
        period_names <- c(period_names, paste0("Период ", i, sep = ""))
      }
      names(tf_idf_only) <- period_names
      # names(tf_idf_only) <- c("Период 1", "Период 2")
      cos.mat <- cosine(as.matrix(tf_idf_only))  # Removes the first column for cosine calculation
      # ifelse(max(tdm_df_with_dynamism$freq_all) != 0, max(tdm_df_with_dynamism$freq_all), 1)  значит следующее.
      # Если max(tdm_df_with_dynamism$freq_all) != 0, то вернуть max(tdm_df_with_dynamism$freq_all),
      # иначе вернуть 1.
      tdm_df_with_dynamism$freq_all_normalized <- (tdm_df_with_dynamism$freq_all) / 
        ifelse(max(tdm_df_with_dynamism$freq_all) != 0, max(tdm_df_with_dynamism$freq_all), 1)  
      tdm_df_with_dynamism$dynamism_normalized <- (tdm_df_with_dynamism$dynamism) / 
        ifelse(max(tdm_df_with_dynamism$dynamism) != 0, max(tdm_df_with_dynamism$dynamism), 1)  
      tdm_df_with_dynamism$freq_all_and_dynamism_normalized <- 
        tdm_df_with_dynamism$dynamism_normalized + tdm_df_with_dynamism$freq_all_normalized
      # Сортировка датафрейма по столбцу freq_all_and_dynamism_normalized по убыванию
      tdm_df_with_dynamism <- 
        tdm_df_with_dynamism[order(tdm_df_with_dynamism$freq_all_and_dynamism_normalized, 
                                   decreasing = TRUE),] 
      
      
      
      output$compareFilesTable <- renderTable({
        cos_mat_reactive_tf(cos.mat)
        cos.mat
      })
      # output$dynamicPlotAll <- renderPlot({
      #   # Вывод всех слов на графике, кроме тех, которые пересекаются
      #   # При этом подписываются некоторые слова, хотя точки на графике есть для всех слов.
      #   dynamicPlotAll <- ggplot(tdm_df_with_dynamism, aes(x = dynamism, y = freq_all, label = word)) +
      #     geom_point() +
      #     geom_text_repel(max.overlaps = 10, max.time = 0.2) +
      #     labs(x = "Динамика", y = "Значимость", title = "Тренд-карта для всех слов") +
      #     theme_minimal()
      #   return(dynamicPlotAll)
      # })
      
     
      output$trendMapLimited <- renderPlot({
        # Вывод графика для WORDS_COUNT_IN_TRENDMAP слов без пересечений слов на графике. 
        # При этом подписываются некоторые слова, хотя точки на графике есть для всех слов.
        
        
        # Нормализация данных для отображения точек на 
        # отрезки [0, 1] для WORDS_COUNT_IN_TRENDMAP слов
        
        # Выделение WORDS_COUNT_IN_TRENDMAP слов с наибольшими значениями sum_of_rake_all_norm_and_dyn_norm 
        tdm_df_with_dynamism_limited <- tdm_df_with_dynamism[1:WORDS_COUNT_IN_TRENDMAP, ]
        
        
        # Нормализация динамики для WORDS_COUNT_IN_TRENDMAP слов
        
        # Нужно сместить все значения динамики, чтобы их минимум был в 0.
        # Если минимум отрицательный, то при его вычитании из остальных значений
        # новый минимум окажется в нуле (так как минус на минус дает плюс).
        # Если минимум положительный, то при его вычитании из остальных значений
        # новый минимум так же окажется в нуле.
        tdm_df_with_dynamism_limited$dynamism_shifted_for_30 <- 
          tdm_df_with_dynamism_limited$dynamism - min(tdm_df_with_dynamism_limited$dynamism)
        
        
        # После смещения все значения делятся на новый максимум, 
        # чтобы отобразить все значения динамики на отрезок [0; 1].
        tdm_df_with_dynamism_limited$dynamism_normalized_for_30 <- 
          tdm_df_with_dynamism_limited$dynamism_shifted_for_30 /
          ifelse(max(tdm_df_with_dynamism_limited$dynamism_shifted_for_30) != 0,
                 max(tdm_df_with_dynamism_limited$dynamism_shifted_for_30), 1)
        
        
        # Нормализация частоты встречамости для WORDS_COUNT_IN_TRENDMAP слов
        
        # tdm_df_with_dynamism_limited$freq_all >= 0.
        # Нужно сместить все значения freq_all, чтобы их минимум был в 0.
        # freq_all >= 0. Значит при вычитании минимума из всех значенией, 
        # новый минимум окажется в нуле.
        tdm_df_with_dynamism_limited$freq_all_shifted_for_30 <- 
          tdm_df_with_dynamism_limited$freq_all - min(tdm_df_with_dynamism_limited$freq_all)
        
        # После смещения все значения делятся на новый максимум, 
        # чтобы отобразить все значения rake_all на отрезок [0; 1].
        tdm_df_with_dynamism_limited$freq_all_normalized_for_30 <- 
          (tdm_df_with_dynamism_limited$freq_all_shifted_for_30) /
          ifelse(max(tdm_df_with_dynamism_limited$freq_all_shifted_for_30) != 0,
                 max(tdm_df_with_dynamism_limited$freq_all_shifted_for_30), 1)
        
        
        
        # Смещение оси координат так, чтобы все значения динамики были >= 0. 
        # Для этого для всех выводимых слов к значениям динамики 
        # прибавляют модуль минимального значения динамики
        trendMapLimited <- ggplot(tdm_df_with_dynamism_limited[1:WORDS_COUNT_IN_TRENDMAP, ], 
                                     aes(x = dynamism_normalized_for_30, 
                                         y = freq_all_normalized_for_30, 
                                         label = word)) +
          geom_point() + # creates scatterplot (не нашел быстро перевод на русский)
          # geom_label_repel - аналог для geom_text_repel, но с рамкой вокруг каждой фразы (слова)
          # geom_label_repel(max.overlaps = 10, label.size = 0.7) + # 
          # max.overlaps - исключает слова, которые пересекаются слишком много раз;
          # size - размер выводимых слов;
          # force - сила отталкивания пересекающихся слов (по умолчанию 1).
          geom_text_repel(force = 2, size = 7) +
          labs(x = "Динамика", y = "Значимость", title = paste0("Тренд-карта")) +
          theme_classic(base_size = 26)
        ggsave("30 слов TF.png", plot = trendMapLimited, width = 8, height = 6, dpi = 300)
        trend_map_plot_tf(trendMapLimited)
        # Для перевода тренд-карты на английский язык
        trend_map_df_tf(tdm_df_with_dynamism_limited[1:WORDS_COUNT_IN_TRENDMAP, 
                                                     c("word", "dynamism_normalized_for_30", 
                                                       "freq_all_normalized_for_30")])
        
        showNotification(LABEL_CALCULATIONS_COMPLETED, duration = TIME_OF_NOTIFICATION_DURATION)
        return(trendMapLimited)
      })
    }
  }
  
  
  ObserveEventCompareFilesBtnRake <- function() {
    d_all <- Filter(Negate(is.null), list(files_preprocessed_data_rake[["df_1"]],
                                          files_preprocessed_data_rake[["df_2"]],
                                          files_preprocessed_data_rake[["df_3"]],
                                          files_preprocessed_data_rake[["df_4"]],
                                          files_preprocessed_data_rake[["df_5"]],
                                          files_preprocessed_data_rake[["df_6"]]))
    cos.mat <- NULL
    if (length(d_all) <= 1) {
      showNotification(LABEL_NEED_MORE_PROCESSED_FILES,
                       duration = TIME_OF_NOTIFICATION_DURATION)
    }
    else
    {
      showNotification(LABEL_CALCULATIONS_IN_PROGRESS, duration = TIME_OF_NOTIFICATION_DURATION)
      amount_of_processed_files <- length(d_all)

      # d_all[[i]] содержит таблицу со столбцами keyword ngram freq rake
      # Чтобы full_join происходил корректно,
      # и чтобы не нужно было переименовывать после него столбцы,
      # нужно, чтобы все имена столбцов, кроме столбца,
      # по которому происходит соединение, были разными.
      # В данном случае для таблицы с номером i столбцы будут следующими:
      # keyword freqi rakei.

      # if (length(d_all) == 2) {

        for (i in  1:amount_of_processed_files)
        {
          # show(names(d_all[[i]]))
          names(d_all[[i]]) <- c("keyword", paste("ngram", i, sep = ''), 
                                 paste("freq", i, sep = ""), 
                                 paste("rake", i, sep = ""))
          # show(names(d_all[[i]]))
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
          # col_names_keyword_freq_tf <- c(col_names_keyword_freq_tf,  paste("freq", i, sep = ""), paste("tf", i, sep = ""))
          col_names_keyword_rake <- c(col_names_keyword_rake, paste("rake", i, sep = ""))
          col_names_rake <- c(col_names_rake, paste("rake", i, sep = ""))
        }

       # d_all <- full_join(d_all[[1]], d_all[[2]], by='keyword')
        # d_all <- d_all %>% replace(is.na (.), 0)
        #rake_df <- select(d_all, 'keyword', 'rake.x', 'rake.y')
        # names(rake_df) <- c('keyword', 'rake1', 'rake2')

        rake_df <- select(d_all, 'keyword', all_of(col_names_keyword_rake))
        rake_df_only <- select(d_all, all_of(col_names_rake))
        # tf_idf_only <- select(tf_idf, 'tf_idf1', 'tf_idf2')
        period_names <- c()
        for (i in 1:amount_of_processed_files)
        {
          period_names <- c(period_names, paste0("Период ", i, sep = ""))
        }
        names(rake_df_only) <- period_names
        # show(rake_df_only)
        # tdm_df <- select(d_all, 'word', 'freq.x', 'freq.y')
        # names(tdm_df) <- c('word', 'freq1', 'freq2')
        # tdm_df <- tdm_df %>% mutate(num_of_occurrences = rowSums(select(tdm_df, 'freq1', 'freq2') != 0))
        # tdm_df <- tdm_df %>% mutate(idf = log(4 / (1 + num_of_occurrences) + 1))
        cos.mat <- cosine(as.matrix(rake_df_only))
        rake_df_with_dynamism <- rake_df

        rake_df_with_dynamism$rake_all <- rake_df_with_dynamism[['rake1']]
        for (i in 2:amount_of_processed_files)
        {
          rake_df_with_dynamism$rake_all <- rake_df_with_dynamism$rake_all +
            rake_df_with_dynamism[[paste("rake", i, sep = "")]]
        }

        # rake_df_with_dynamism$rake_all <- rake_df_with_dynamism$rake1 + rake_df_with_dynamism$rake2

        # ???
        # tdm_df_with_dynamism$dynamism <- (tdm_df_with_dynamism$freq2 - tdm_df_with_dynamism$freq1) / ifelse(tdm_df_with_dynamism$freq1 != 0, tdm_df_with_dynamism$freq1, 1)

        # Средний абсолютный прирост
        rake_df_with_dynamism$dynamism <- rake_df_with_dynamism[[paste("rake", 
                                                                       amount_of_processed_files, 
                                                                       sep = "")]]
        for (i in (amount_of_processed_files - 1):1)
        {
          rake_df_with_dynamism$dynamism <- (rake_df_with_dynamism$dynamism -
            rake_df_with_dynamism[["rake1"]]) / (amount_of_processed_files - 1)
        }
      # # ifelse(max(tdm_df_with_dynamism$freq_all) != 0, max(tdm_df_with_dynamism$freq_all), 1)  значит следующее.
      # # Если max(tdm_df_with_dynamism$freq_all) != 0, то вернуть max(tdm_df_with_dynamism$freq_all),
      # # иначе вернуть 1.
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
      # # Сортировка датафрейма по столбцу freq_all_and_dynamism_normalized по убыванию
      rake_df_with_dynamism <- rake_df_with_dynamism[order(rake_df_with_dynamism$sum_of_rake_all_norm_and_dyn_norm, decreasing = TRUE),]
      output$compareFilesTable <- renderTable({
        # cos_mat_for_output <- cos.mat
        # ggsave("RAKE Cos.png", plot = cos_mat_for_output, width = 8, height = 6, dpi = 300)
        # cos_mat_for_output
        cos_mat_reactive_rake(cos.mat)
        cos.mat
      })
      # output$dynamicPlotAll <- renderPlot({
      #   # Вывод всех слов на графике, кроме тех, которые пересекаются
      #   # При этом подписываются некоторые слова, хотя точки на графике есть для всех слов.
      #   plot_all <- ggplot(rake_df_with_dynamism, aes(x = dynamism, y = rake_all, label = keyword)) +
      #     geom_point() +
      #     geom_text_repel(max.overlaps = 10, max.time = 0.2) +
      #     labs(x = "Динамика", y = "Значимость", title = "Тренд-карта для всех слов") +
      #     # theme_minimal()
      #     theme_classic()
      #   # Сохранение графика в директорию с запускаемой программой
      #   ggsave("Все слова.png", plot = plot_all, width = 8, height = 6, dpi = 300)
      #   return(plot_all)
      # })
      output$trendMapLimited <- renderPlot({
        # Вывод графика для WORDS_COUNT_IN_TRENDMAP слов без пересечений слов на графике.
        # При этом подписываются некоторые слова, хотя точки на графике есть для всех слов.

        # Нормализация данных для отображения точек на
        # отрезки [0, 1] для 30 слов

        # Выделение 30 слов с наибольшими значениями sum_of_rake_all_norm_and_dyn_norm
        rake_df_with_dynamism_limited <- rake_df_with_dynamism[1:WORDS_COUNT_IN_TRENDMAP, ]


        # Нормализация динамики для 30 слов

        # Нужно сместить все значения динамики, чтобы их минимум был в 0.
        # Если минимум отрицательный, то при его вычитании из остальных значений
        # новый минимум окажется в нуле (так как минус на минус дает плюс).
        # Если минимум положительный, то при его вычитании из остальных значений
        # новый минимум так же окажется в нуле.
        rake_df_with_dynamism_limited$dynamism_shifted_for_30 <- rake_df_with_dynamism_limited$dynamism -
          min(rake_df_with_dynamism_limited$dynamism)


        # После смещения все значения делятся на новый максимум,
        # чтобы отобразить все значения динамики на отрезок [0; 1].
        rake_df_with_dynamism_limited$dynamism_normalized_for_30 <- rake_df_with_dynamism_limited$dynamism_shifted_for_30 /
          ifelse(max(rake_df_with_dynamism_limited$dynamism_shifted_for_30) != 0,
                 max(rake_df_with_dynamism_limited$dynamism_shifted_for_30), 1)


        # Нормализация rake_all для 30 слов

        # rake_df_with_dynamism_limited$rake_all >= 0.
        # Нужно сместить все значения rake_all, чтобы их минимум был в 0.
        # rake_all >= 0. Значит при вычитании минимума из всех значенией,
        # новый минимум окажется в нуле.
        rake_df_with_dynamism_limited$rake_all_shifted_for_30 <- rake_df_with_dynamism_limited$rake_all - min(rake_df_with_dynamism_limited$rake_all)

        # После смещения все значения делятся на новый максимум,
        # чтобы отобразить все значения rake_all на отрезок [0; 1].
        rake_df_with_dynamism_limited$rake_all_normalized_for_30 <- (rake_df_with_dynamism_limited$rake_all_shifted_for_30) /
          ifelse(max(rake_df_with_dynamism_limited$rake_all_shifted_for_30) != 0,
                 max(rake_df_with_dynamism_limited$rake_all_shifted_for_30), 1)
        
        # В rake_df_with_dynamism_limited находятся столбцы: keyword, rake1, ..., raken,
        # rake_all, dynamism, rake_all_normalized, dynamism_normalized, sum_of_rake_all_norm_and_dyn_norm,
        # dynamism_shifted_for_30, dynamism_normalized_for_30, rake_all_shifted_for_30, rake_all_normalized_for_30
        plot_limited <- ggplot(rake_df_with_dynamism_limited, aes(x = dynamism_normalized_for_30, y = rake_all_normalized_for_30, label = keyword)) +
          geom_point() + # creates scatterplot (не нашел быстро перевод на русский)
          # geom_label_repel - аналог для geom_text_repel, но с рамкой вокруг каждой фразы (слова)
          # geom_label_repel(max.overlaps = 10, label.size = 0.7) + # 
          # max.overlaps - исключает слова, которые пересекаются слишком много раз;
          # size - размер выводимых слов;
          # force - сила отталкивания пересекающихся слов (по умолчанию 1).
          geom_text_repel(force = 2, size = 7) +
          labs(x = "Динамика", y = "Значимость", title = paste0("Тренд-карта")) +
          theme_classic(base_size = 26)
        # Сохранение графика в директорию с запускаемой программой
        # Для width и height значение 1 значит 300 пискселей, 2 - 600, ...
        trend_map_plot_rake(plot_limited)
        trend_map_df_rake(data.frame(rake_df_with_dynamism_limited[,c("keyword", "dynamism_normalized_for_30", "rake_all_normalized_for_30")]))
        View(data.frame(rake_df_with_dynamism_limited[,c("keyword", "dynamism_normalized_for_30", "rake_all_normalized_for_30")]))
        ggsave(paste0(WORDS_COUNT_IN_TRENDMAP, " слов RAKE.png"), plot = plot_limited, width = 8, height = 6, dpi = 300)
        
        showNotification(LABEL_CALCULATIONS_COMPLETED, duration = TIME_OF_NOTIFICATION_DURATION)
        return(plot_limited)
      })
    }
  }
  
  # saved_plot <- reactiveVal(NULL)
  # 
  # observeEvent(input$create_plot, {
  #   # Генерируем график и сохраняем его
  #   p <- ggplot(mtcars, aes(mpg, wt)) + geom_point()
  #   saved_plot(p)
  # })
  # 
  # output$plot_output <- renderPlot({
  #   req(saved_plot())
  #   saved_plot()
  # })
  # 
  # output$download_plot <- downloadHandler(
  #   filename = function() {
  #     paste0("my_plot_", Sys.time(), ".png")
  #   },
  #   content = function(file) {
  #     ggsave(file, plot = saved_plot(), width = 6, height = 4)
  #   }
  # )
  
  
  # output$downloadData1 <- downloadHandler(
  #   filename = function() {
  #     paste("data1-", Sys.time(), ".zip", sep="")
  #   },
  #   content = function(file) {
  #     tmpdir <- tempdir()
  #     oldwd <- setwd(tmpdir)
  #     on.exit(setwd(oldwd))
  #     files_to_zip <- character(0)  
  #     # f1_rake <- f2_rake <- f3_rake <- f1_tf <- f2_tf <- f3_tf <- NULL
  #     
  #     #if (!is.null(wordcloud_data_rake_1()) )
  #     if (input$radio == 2 )
  #       # && !is.na(wordcloud_data_rake_1()) && 
  #       # !is.na(barplot_rake_1()) && !is.na(word_table_rake_1())
  #     {
  #       req(wordcloud_data_rake_1(), barplot_rake_1(), word_table_rake_1())
  #       f1_rake <- file.path(tmpdir, "wordcloud_rake.png")
  #       f2_rake <- file.path(tmpdir, "barPlot_rake.png")
  #       f3_rake <- file.path(tmpdir, "table_rake.csv")
  #       png(f1_rake);
  #       wordcloud2(wordcloud_data_rake_1(), size = 0.45)
  #       dev.off()
  #       png(f2_rake);
  #       print(barplot_rake_1())
  #       dev.off()
  #       write.csv(word_table_rake_1(), f3_rake, row.names = FALSE)
  #       zip::zip(zipfile = file, files = c(files_to_zip, f1_rake, f2_rake, f3_rake), mode = "cherry-pick")
  #     }
  #     if (input$radio == 1 )
  #       # && !is.null(wordcloud_data_tf_1()) && 
  #       # !is.null(barplot_tf_1()) && !is.null(word_table_tf_1())
  #     # if(!is.null(wordcloud_data_tf_1()))
  #       
  #     {
  #       # show("ssdfdsf")
  #       # browser()
  #       req(wordcloud_data_tf_1(), barplot_tf_1(), word_table_tf_1())
  #       f1_tf <- file.path(tmpdir, "wordcloud_tf.png")
  #       f2_tf <- file.path(tmpdir, "barPlot_tf.png")
  #       f3_tf <- file.path(tmpdir, "table_tf.csv")
  #       png(f1_tf);
  #       show(wordcloud_data_tf_1())
  #       wordcloud2(wordcloud_data_tf_1(), size = 0.45)
  #       dev.off()
  #       png(f2_tf);
  #       print(barplot_tf_1())
  #       dev.off()
  #       show(word_table_tf_1())
  #       write.csv(word_table_tf_1(), f3_tf, row.names = FALSE)
  #       zip::zip(zipfile = file, files = c(files_to_zip, f1_tf, f2_tf, f3_tf), mode = "cherry-pick")
  #     }
  #     # if (!is.null(wordcloud_data_rake_1()) && !is.null(wordcloud_data_tf_1()))
  #     # {
  #     #   zip::zip(zipfile = file, files = c(f1_rake, f2_rake, f3_rake, f1_tf, f2_tf, f3_tf), mode = "cherry-pick")
  #     # }
  #     #  
  #     # else if(!is.null(wordcloud_data_rake_1()))
  #     # {
  #     #   zip::zip(zipfile = file, files = c(f1_rake, f2_rake, f3_rake), mode = "cherry-pick")
  #     # }
  #     # else {
  #     #   zip::zip(zipfile = file, files = c(f1_tf, f2_tf, f3_tf), mode = "cherry-pick")
  #     # }
  #             
  #   }
  # )
  
  
  output$downloadData1 <- downloadHandler(
    filename = function() {
      paste("data1-", Sys.time(), ".zip", sep = "")
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
          # Проверяем наличие данных для RAKE
          if (!is.null(wordcloud_data_rake_1()) && 
              !is.null(barplot_rake_1()) && 
              !is.null(word_table_rake_1())) {
            
            f1_rake <- "wordcloud_rake.png"
            f2_rake <- "barPlot_rake.png"
            f3_rake <- "table_rake.csv"
            
            # Сохраняем wordcloud
            png(f1_rake, width = 800, height = 600)
            print(wordcloud(wordcloud_data_rake_1()[["keyword"]], wordcloud_data_rake_1()[["freq"]], min.freq = 0))
            # print(wordcloud(wordcloud_data_rake_1()[["keyword"]], 
            #                 wordcloud_data_rake_1()[["freq"]], 
            #                 min.freq = 0,
            #                 colors = brewer.pal(8, "Dark2")))
            dev.off()
            
            # Сохраняем barplot
            png(f2_rake, width = 800, height = 600)
            print(barplot_rake_1())
            dev.off()
            
            # Сохраняем таблицу
            write.csv(word_table_rake_1(), f3_rake, row.names = FALSE)
            
            files_to_zip <- c(f1_rake, f2_rake, f3_rake)
            valid_files <- TRUE
          }
        } 
        else if (input$radio == 1) {
          # Проверяем наличие данных для TF
          if (!is.null(wordcloud_data_tf_1()) && 
              !is.null(barplot_tf_1()) && 
              !is.null(word_table_tf_1())) {
            
            f1_tf <- "wordcloud_tf.png"
            f2_tf <- "barPlot_tf.png"
            f3_tf <- "table_tf.csv"
            
            # Сохраняем wordcloud
            png(f1_tf, width = 800, height = 600)
            print(wordcloud(wordcloud_data_tf_1()[["word"]], wordcloud_data_tf_1()[["tf"]], , min.freq = 0))
            dev.off()
            
            # Сохраняем barplot
            png(f2_tf, width = 800, height = 600)
            print(barplot_tf_1())
            dev.off()
            
            # Сохраняем таблицу
            write.csv(word_table_tf_1(), f3_tf, row.names = FALSE)
            
            files_to_zip <- c(f1_tf, f2_tf, f3_tf)
            valid_files <- TRUE
          }
        }
        
        if (valid_files && length(files_to_zip) > 0) {
          # Проверяем существование файлов перед архивацией
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
      paste("data2-", Sys.time(), ".zip", sep="")
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
          # Проверяем наличие данных для RAKE
          if (!is.null(wordcloud_data_rake_2()) && 
              !is.null(barplot_rake_2()) && 
              !is.null(word_table_rake_2())) {
            
            f1_rake <- "wordcloud_rake.png"
            f2_rake <- "barPlot_rake.png"
            f3_rake <- "table_rake.csv"
            
            # Сохраняем wordcloud
            png(f1_rake, width = 800, height = 600)
            print(wordcloud(wordcloud_data_rake_2()[["keyword"]], wordcloud_data_rake_2()[["freq"]], min.freq = 0))
            dev.off()
            
            # Сохраняем barplot
            png(f2_rake, width = 800, height = 600)
            print(barplot_rake_2())
            dev.off()
            
            # Сохраняем таблицу
            write.csv(word_table_rake_2(), f3_rake, row.names = FALSE)
            
            files_to_zip <- c(f1_rake, f2_rake, f3_rake)
            valid_files <- TRUE
          }
        } 
        else if (input$radio == 1) {
          # Проверяем наличие данных для TF
          if (!is.null(wordcloud_data_tf_2()) && 
              !is.null(barplot_tf_2()) && 
              !is.null(word_table_tf_2())) {
            
            f1_tf <- "wordcloud_tf.png"
            f2_tf <- "barPlot_tf.png"
            f3_tf <- "table_tf.csv"
            
            # Сохраняем wordcloud
            png(f1_tf, width = 800, height = 600)
            print(wordcloud(wordcloud_data_tf_2()[["word"]], wordcloud_data_tf_2()[["tf"]], min.freq = 0))
            dev.off()
            
            # Сохраняем barplot
            png(f2_tf, width = 800, height = 600)
            print(barplot_tf_2())
            dev.off()
            
            # Сохраняем таблицу
            write.csv(word_table_tf_2(), f3_tf, row.names = FALSE)
            
            files_to_zip <- c(f1_tf, f2_tf, f3_tf)
            valid_files <- TRUE
          }
        }
        
        if (valid_files && length(files_to_zip) > 0) {
          # Проверяем существование файлов перед архивацией
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
      paste("data3-", Sys.time(), ".zip", sep="")
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
          # Проверяем наличие данных для RAKE
          if (!is.null(wordcloud_data_rake_3()) && 
              !is.null(barplot_rake_3()) && 
              !is.null(word_table_rake_3())) {
            
            f1_rake <- "wordcloud_rake.png"
            f2_rake <- "barPlot_rake.png"
            f3_rake <- "table_rake.csv"
            
            # Сохраняем wordcloud
            png(f1_rake, width = 800, height = 600)
            print(wordcloud(wordcloud_data_rake_3()[["keyword"]], wordcloud_data_rake_3()[["freq"]], min.freq = 0))
            dev.off()
            
            # Сохраняем barplot
            png(f2_rake, width = 800, height = 600)
            print(barplot_rake_3())
            dev.off()
            
            # Сохраняем таблицу
            write.csv(word_table_rake_3(), f3_rake, row.names = FALSE)
            
            files_to_zip <- c(f1_rake, f2_rake, f3_rake)
            valid_files <- TRUE
          }
        } 
        else if (input$radio == 1) {
          # Проверяем наличие данных для TF
          if (!is.null(wordcloud_data_tf_3()) && 
              !is.null(barplot_tf_3()) && 
              !is.null(word_table_tf_3())) {
            
            f1_tf <- "wordcloud_tf.png"
            f2_tf <- "barPlot_tf.png"
            f3_tf <- "table_tf.csv"
            
            # Сохраняем wordcloud
            png(f1_tf, width = 800, height = 600)
            print(wordcloud(wordcloud_data_tf_3()[["word"]], wordcloud_data_tf_3()[["tf"]], min.freq = 0))
            dev.off()
            
            # Сохраняем barplot
            png(f2_tf, width = 800, height = 600)
            print(barplot_tf_3())
            dev.off()
            
            # Сохраняем таблицу
            write.csv(word_table_tf_3(), f3_tf, row.names = FALSE)
            
            files_to_zip <- c(f1_tf, f2_tf, f3_tf)
            valid_files <- TRUE
          }
        }
        
        if (valid_files && length(files_to_zip) > 0) {
          # Проверяем существование файлов перед архивацией
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
      paste("data4-", Sys.time(), ".zip", sep="")
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
          # Проверяем наличие данных для RAKE
          if (!is.null(wordcloud_data_rake_4()) && 
              !is.null(barplot_rake_4()) && 
              !is.null(word_table_rake_4())) {
            
            f1_rake <- "wordcloud_rake.png"
            f2_rake <- "barPlot_rake.png"
            f3_rake <- "table_rake.csv"
            
            # Сохраняем wordcloud
            png(f1_rake, width = 800, height = 600)
            print(wordcloud(wordcloud_data_rake_4()[["keyword"]], wordcloud_data_rake_4()[["freq"]], min.freq = 0))
            dev.off()
            
            # Сохраняем barplot
            png(f2_rake, width = 800, height = 600)
            print(barplot_rake_4())
            dev.off()
            
            # Сохраняем таблицу
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
            
            # Сохраняем wordcloud
            png(f1_tf, width = 800, height = 600)
            print(wordcloud(wordcloud_data_tf_4()[["word"]], wordcloud_data_tf_4()[["tf"]], min.freq = 0))
            dev.off()
            
            # Сохраняем barplot
            png(f2_tf, width = 800, height = 600)
            print(barplot_tf_4())
            dev.off()
            
            # Сохраняем таблицу
            write.csv(word_table_tf_4(), f3_tf, row.names = FALSE)
            
            files_to_zip <- c(f1_tf, f2_tf, f3_tf)
            valid_files <- TRUE
          }
        }
        
        if (valid_files && length(files_to_zip) > 0) {
          # Проверяем существование файлов перед архивацией
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
      paste("data-", Sys.time(), ".zip", sep="")
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
          # Проверяем наличие данных для RAKE
          if (!is.null(wordcloud_data_rake_5()) && 
              !is.null(barplot_rake_5()) && 
              !is.null(word_table_rake_5())) {
            
            f1_rake <- "wordcloud_rake.png"
            f2_rake <- "barPlot_rake.png"
            f3_rake <- "table_rake.csv"
            
            # Сохраняем wordcloud
            png(f1_rake, width = 800, height = 600)
            print(wordcloud(wordcloud_data_rake_5()[["keyword"]], wordcloud_data_rake_5()[["freq"]], min.freq = 0))
            dev.off()
            
            # Сохраняем barplot
            png(f2_rake, width = 800, height = 600)
            print(barplot_rake_5())
            dev.off()
            
            # Сохраняем таблицу
            write.csv(word_table_rake_5(), f3_rake, row.names = FALSE)
            
            files_to_zip <- c(f1_rake, f2_rake, f3_rake)
            valid_files <- TRUE
          }
        } 
        else if (input$radio == 1) {
          # Проверяем наличие данных для TF
          if (!is.null(wordcloud_data_tf_5()) && 
              !is.null(barplot_tf_5()) && 
              !is.null(word_table_tf_5())) {
            
            f1_tf <- "wordcloud_tf.png"
            f2_tf <- "barPlot_tf.png"
            f3_tf <- "table_tf.csv"
            
            # Сохраняем wordcloud
            png(f1_tf, width = 800, height = 600)
            print(wordcloud(wordcloud_data_tf_1()[["word"]], wordcloud_data_tf_1()[["tf"]], min.freq = 0))
            dev.off()
            
            # Сохраняем barplot
            png(f2_tf, width = 800, height = 600)
            print(barplot_tf_5())
            dev.off()
            
            # Сохраняем таблицу
            write.csv(word_table_tf_5(), f3_tf, row.names = FALSE)
            
            files_to_zip <- c(f1_tf, f2_tf, f3_tf)
            valid_files <- TRUE
          }
        }
        
        if (valid_files && length(files_to_zip) > 0) {
          # Проверяем существование файлов перед архивацией
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
  
  
  output$downloadData6 <- downloadHandler(
    filename = function() {
      paste("data-", Sys.time(), ".zip", sep="")
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
          # Проверяем наличие данных для RAKE
          if (!is.null(wordcloud_data_rake_6()) && 
              !is.null(barplot_rake_6()) && 
              !is.null(word_table_rake_6())) {
            
            f1_rake <- "wordcloud_rake.png"
            f2_rake <- "barPlot_rake.png"
            f3_rake <- "table_rake.csv"
            
            # Сохраняем wordcloud
            png(f1_rake, width = 800, height = 600)
            print(wordcloud(wordcloud_data_rake_6()[["keyword"]], wordcloud_data_rake_6()[["freq"]], min.freq = 0))
            dev.off()
            
            # Сохраняем barplot
            png(f2_rake, width = 800, height = 600)
            print(barplot_rake_6())
            dev.off()
            
            # Сохраняем таблицу
            write.csv(word_table_rake_6(), f3_rake, row.names = FALSE)
            
            files_to_zip <- c(f1_rake, f2_rake, f3_rake)
            valid_files <- TRUE
          }
        } 
        else if (input$radio == 1) {
          # Проверяем наличие данных для TF
          if (!is.null(wordcloud_data_tf_6()) && 
              !is.null(barplot_tf_6()) && 
              !is.null(word_table_tf_6())) {
            
            f1_tf <- "wordcloud_tf.png"
            f2_tf <- "barPlot_tf.png"
            f3_tf <- "table_tf.csv"
            
            # Сохраняем wordcloud
            png(f1_tf, width = 800, height = 600)
            print(wordcloud(wordcloud_data_tf_1()[["word"]], wordcloud_data_tf_1()[["tf"]], min.freq = 0))
            dev.off()
            
            # Сохраняем barplot
            png(f2_tf, width = 800, height = 600)
            print(barplot_tf_6())
            dev.off()
            
            # Сохраняем таблицу
            write.csv(word_table_tf_6(), f3_tf, row.names = FALSE)
            
            files_to_zip <- c(f1_tf, f2_tf, f3_tf)
            valid_files <- TRUE
          }
        }
        
        if (valid_files && length(files_to_zip) > 0) {
          # Проверяем существование файлов перед архивацией
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
      paste("data-", Sys.time(), ".zip", sep="")
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
          # Проверяем наличие данных для RAKE
          if (!is.null(cos_mat_reactive_rake()) && 
              !is.null(trend_map_plot_rake())) { 
            # TODO: Стоит ли добавить !is.null(trend_map_df_rake() в условие?
            # Для перевода тренд-карт на английский язык
            file_name_df <- "trend_map_rake.csv" 
            write.csv(trend_map_df_rake(), file_name_df, row.names = FALSE, col.names = FALSE)
            
            file_name_cos <- "cos_matrix_rake.csv"
            file_name_plot <- "trend_map_rake.png"
            write.csv(cos_mat_reactive_rake(), file_name_cos, row.names = FALSE, col.names = FALSE)
            
            # Открывает графическое устройство для записи в файл file_name_plot
            png(file_name_plot, width = 800, height = 600)
            # Явно выводит (печатает) графический объект, возвращаемый функцией trend_map_plot_rake()
            print(trend_map_plot_rake())
            # Закрывает графическое устройство, завершая сохранение файла. 
            # Без этого вызова файл может остаться повреждённым или пустым.
            dev.off()

            files_to_zip <- c(file_name_cos, file_name_plot, file_name_df)
            valid_files <- TRUE
          }
        } 
        else if (input$radio == 1) {
          # Проверяем наличие данных для TF
          if (!is.null(cos_mat_reactive_tf()) && 
              !is.null(trend_map_plot_tf())) {
            # TODO: Стоит ли добавить !is.null(trend_map_df_tf() в условие?
            # Для перевода тренд-карт на английский язык
            file_name_df <- "trend_map_tf_idf.csv" 
            write.csv(trend_map_df_tf(), file_name_df, row.names = FALSE, col.names = FALSE)
            
            file_name_cos <- "cos_matrix_tf_idf.csv"
            file_name_plot <- "trend_map_tf_idf.png"
            write.csv(cos_mat_reactive_tf(), file_name_cos, row.names = FALSE, col.names = FALSE)
            
            # Открывает графическое устройство для записи в файл file_name_plot
            png(file_name_plot, width = 800, height = 600)
            # Явно выводит (печатает) графический объект, возвращаемый функцией trend_map_plot_rake()
            print(trend_map_plot_tf())
            # Закрывает графическое устройство, завершая сохранение файла. 
            # Без этого вызова файл может остаться повреждённым или пустым.
            dev.off()
            
            files_to_zip <- c(file_name_cos, file_name_plot, file_name_df)
            valid_files <- TRUE
          }
        }
        
        if (valid_files && length(files_to_zip) > 0) {
          # Проверяем существование файлов перед архивацией
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
  observeEvent(input$analyze6, {
    if (input$radio == 1) 
    {
      files_preprocessed_data_frequency[["df_6"]] <- AnalyzeAndRenderFrequency(input[["file6"]], "barPlot6", "wordTable6", "wordcloud6")
    }
    if (input$radio == 2)
    {
      files_preprocessed_data_rake[["df_6"]] <- AnalyzeAndRenderRake(input[["file6"]], "barPlot6", "wordTable6", "wordcloud")
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
