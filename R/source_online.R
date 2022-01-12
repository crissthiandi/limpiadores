time_to_time <- function(base_xd){
  base_xd %<>%
    mutate(
      `Marca temporal` = as.POSIXlt.character(`Marca temporal`)
    ) %>%
    mutate(
      Fecha = as.Date(`Marca temporal`)
    ) %>%
    select(!`Marca temporal`) %>%
    mutate(
      across(!all_of(c("Fecha","CANAL","NOMBRE")),
             ~ as.character(.x))
    ) %>%
    pivot_longer(cols = !c(Fecha,CANAL,NOMBRE),
                 names_to = "Tipo_plus_Marca",
                 values_to = "Precio_sucio") %>%
    select(Fecha,CANAL,Tipo_plus_Marca,Precio_sucio)
  return(base_xd)
}

Depurandoando_texto <- function(base_xd){
  estraer_marca <- function(vector,inicia = 2){
    str_locate(str_sub(vector,inicia,-1),"[A-Z]")[1]
  }

  extractor <- function(vector,pos_ini){
    price_text <- str_sub(vector,pos_ini,-1)
    pos_final <- str_locate(str_sub(price_text,1,-1),"[ ]")[1]
    pos_final <- ifelse(is.na(pos_final) && !is.na(pos_ini),0,pos_final)
    maybe_a_price <- str_sub(price_text,1,pos_final-1)
    return(maybe_a_price)
  }

  extraer_precio <- function(vector,inicia = 1){
    vector <- str_replace_all(vector,"\\$ ","$")
    pos_ini <- str_locate(str_sub(vector,inicia,-1),"[\\$]")[1]
    maybe_a_price <- extractor(vector = vector,pos_ini = pos_ini)
    return(maybe_a_price)
  }

  estima_precio <- function(vector,precio,inicia = 1){
    if (is.na(precio)) {
      pos_ini <- str_locate(str_sub(vector,inicia,-1),"[1-9]")[1]
      maybe_a_price <- extractor(vector = vector,pos_ini = pos_ini)
      return(maybe_a_price)
    }else{
      return(precio)
    }
  }

  base_xd %<>%
    mutate(
      posible_marca_num =purrr::pmap_dbl(list(Tipo_plus_Marca),estraer_marca)
    ) %>%
    mutate(
      posible_marca = str_sub(Tipo_plus_Marca,posible_marca_num+1),
      posible_Tipo = str_sub(Tipo_plus_Marca,1,posible_marca_num-1)
    ) %>% select(!posible_marca_num)
  base_xd %<>%
    mutate(
      posible_precio = purrr::pmap_chr(list(Precio_sucio),extraer_precio)
    )
  base_xd %<>%
    mutate(
      posible_precio = purrr::pmap_chr(
        list(Precio_sucio,posible_precio),estima_precio
      )
    )

  return(base_xd)
}

clean_marcas <- function(datos){
  # obtener los factores y modificarlos
  datos <- datos %>%
    mutate(
      CANAL = as.factor(CANAL),
      posible_marca = as.factor(toupper(posible_marca)),
      posible_Tipo = as.factor(toupper(posible_Tipo))
    ) %>%
    mutate(
      CANAL = fct_recode(CANAL,c(`SALLY BEAUTY` = "SALLY BEATY")),
      posible_marca = fct_relabel(posible_marca,~case_when(
        str_detect(.x,"(BISSU|BISSÚ)") ~ "BISSU",
        str_detect(.x,"ESSENCE") ~ "ESSENCE",
        str_detect(.x,"FENTY") ~ "FENTY",
        str_detect(.x,"CATRICE") ~ "CATRICE",
        str_detect(.x,"ESSIE") ~ "ESSIE",
        str_detect(.x,"KLEAN COLOR") ~ "KLEAN COLOR",
        str_detect(.x,"OPY") ~ "OPY",
        str_detect(.x,"SALLY HANSEN") ~ "SALLY HANSEN",
        str_detect(.x,"AVENE") ~ "AVENE",
        str_detect(.x,"BIODERMA") ~ "BIODERMA",
        str_detect(.x,"CERAVE") ~ "CERAVE",
        str_detect(.x,"EUCERIN") ~ "EUCERIN",
        str_detect(.x,"ROCHE POSAY") ~ "LA ROCHE POSAY",
        str_detect(.x,"NEUTROGENA") ~ "NEUTROGENA",
        str_detect(.x,"VICHY") ~ "VICHY",
        str_detect(.x,"KISS") ~ "KISS",
        str_detect(.x,"CLINIQUE") ~ "CLINIQUE",
        str_detect(.x,"ELF") ~ "ELF",
        str_detect(.x,"FENTY") ~ "FENTY",
        str_detect(.x,"MAC") ~ "MAC",
        str_detect(.x,"MAYBELLINE") ~ "MAYBELLINE",
        str_detect(.x,"MILANI") ~ "MILANI",
        str_detect(.x,"NYX.+PRO") ~ "NYX PROF",
        #cadena que le sigue un texto condicional if
        str_detect(.x,"NYX") ~ "NYX",
        str_detect(.x,"MOMIJI") ~ "MOMIJI",
        str_detect(.x,"LOREAL.+PROF") ~ "LOREAL PROFESSIONAL",
        str_detect(.x,"LOREAL.+PARIS") ~ "LOREAL PARIS",
        str_detect(.x,"(L\\'OREAL)") ~ "LOREAL",
        str_detect(.x,"REVLON") ~ "REVLON",
        str_detect(.x,"(WET N WILD|WETNWILD)") ~ "WETNWILD",
        str_detect(.x,"YUYA") ~ "YUYA",
        TRUE ~ .x
      )
      )
    )

  return(datos)
}


# .pkgglobalenv_to_cpp <- new.env(parent=emptyenv())
.pkgglobalenv_to_cpp <- new.env(parent = globalenv())

assign("time_to_time", time_to_time, envir=.pkgglobalenv_to_cpp)
assign("clean_marcas", clean_marcas, envir=.pkgglobalenv_to_cpp)
assign("Depurandoando_texto", Depurandoando_texto,
       envir=.pkgglobalenv_to_cpp)

cleaner_env <- function(){
  cat(ls(.GlobalEnv))
  try({
    rm(list = ls(.GlobalEnv)[which(ls(.GlobalEnv) %in% c("clean_marcas","time_to_time","Depurandoando_texto","read_Dir"))],
       envir = .GlobalEnv)
  })
}

cleaner_env()
