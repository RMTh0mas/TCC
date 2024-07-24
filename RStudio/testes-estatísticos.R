if(!require(dplyr)) install.packages("dplyr") # Instala��o do pacote caso n�o esteja instalado
library(dplyr)                                # Carregamento do pacote
if(!require(RVAideMemoire)) install.packages("RVAideMemoire") # Instala��o do pacote caso n�o esteja instalado
library(RVAideMemoire)                                        # Carregamento do pacote
if(!require(car)) install.packages("car") # Instala��o do pacote caso n�o esteja instalado
library(car)   


if(!require(dplyr)) install.packages("dplyr") # Instala��o do pacote caso n�o esteja instalado
library(dplyr)                                # Carregamento do pacote
if(!require(rstatix)) install.packages("rstatix") # Instala��o do pacote caso n�o esteja instalado
library(rstatix)                                  # Carregamento do pacote
if(!require(ggplot2)) install.packages("ggplot2") # Instala��o do pacote caso n�o esteja instalado
library(ggplot2)   

if(!require(scales)) install.packages("scales")
library(scales)

install.packages("extrafont")
library(extrafont)

windowsFonts(Times=windowsFont("Times New Roman"))

font_import(pattern = "times new roman")
loadfonts(device = "win")

dados1 <- read.csv('C:/Users/renan/Downloads/Resultados.csv', sep = ',', dec = ',')

View(dados1)
glimpse(dados1)


# Verificação de normalidade para cada variável contínua por Arquitetura

# Tempo de Resposta
shapiro.test(dados1$Tempo.de.resposta[dados1$Arquitetura == "Microsserviço"])
shapiro.test(dados1$Tempo.de.resposta[dados1$Arquitetura == "Monolito"])

# Percentual de Erro
shapiro.test(dados1$Percentual.de.erro[dados1$Arquitetura == "Microsserviço"])
shapiro.test(dados1$Percentual.de.erro[dados1$Arquitetura == "Monolito"])

# Latência
shapiro.test(dados1$Latência[dados1$Arquitetura == "Microsserviço"])
shapiro.test(dados1$Latência[dados1$Arquitetura == "Monolito"])

# Taxa de Transferência
shapiro.test(dados1$Taxa.de.transferência[dados1$Arquitetura == "Microsserviço"])
shapiro.test(dados1$Taxa.de.transferência[dados1$Arquitetura == "Monolito"])

# Pacotes Recebidos
shapiro.test(dados1$Pacotes.recebidos[dados1$Arquitetura == "Microsserviço"])
shapiro.test(dados1$Pacotes.recebidos[dados1$Arquitetura == "Monolito"])

# Pacotes Enviados
shapiro.test(dados1$Pacotes.enviados[dados1$Arquitetura == "Microsserviço"])
shapiro.test(dados1$Pacotes.enviados[dados1$Arquitetura == "Monolito"])

#Tempo de resposta
leveneTest(Tempo.de.resposta ~ Arquitetura, data = dados1)

# Percentual de Erro
leveneTest(Percentual.de.erro ~ Arquitetura, data = dados1)

# Latência
leveneTest(Latência ~ Arquitetura, data = dados1)

# Taxa de Transferência
leveneTest(Taxa.de.transferência ~ Arquitetura, data = dados1)

# Pacotes Recebidos
leveneTest(Pacotes.recebidos ~ Arquitetura, data = dados1)

# Pacotes Enviados
leveneTest(Pacotes.enviados ~ Arquitetura, data = dados1)


# Teste T de Welch para todas as variáveis, dado que todas são normais e possuem variâncias heterogêneas

t.test(Tempo.de.resposta ~ Arquitetura, data = dados1, var.equal = FALSE)

# Percentual de Erro
t.test(Percentual.de.erro ~ Arquitetura, data = dados1, var.equal = FALSE)

# Latência
t.test(Latência ~ Arquitetura, data = dados1, var.equal = FALSE)

# Pacotes Recebidos
t.test(Pacotes.recebidos ~ Arquitetura, data = dados1, var.equal = FALSE)

# Pacotes Enviados
t.test(Pacotes.enviados ~ Arquitetura, data = dados1, var.equal = FALSE)

# Taxa de Transferência
t.test(Taxa.de.transferência ~ Arquitetura, data = dados1, var.equal = TRUE)

# Tempo de Resposta

ggplot(dados1, aes(x = Carga.de.usuarios, y = Tempo.de.resposta, color = Arquitetura, group = Arquitetura)) +
  geom_line(size = 1.5) +  # Aumenta a espessura das linhas
  geom_point(size = 4) +  # Aumenta o tamanho dos pontos
  labs(
    x = "Carga de Usuários",
    y = "Tempo de Resposta") +
  theme_minimal() +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, size = 1),  # Reduz a espessura da borda
    legend.position = "bottom",  
    legend.title = element_text(size = 12, colour = "black", family = "Times"),  
    legend.text = element_text(size = 10, colour = "black", family = "Times"),  
    axis.title.x = element_text(colour = "black", family = "Times", size = 13),  # Ajusta o tamanho das fontes dos rótulos do eixo x e y
    axis.title.y = element_text(colour = "black", family = "Times", size = 13),
    axis.text.x = element_text(margin = margin(b = 5)),
    axis.text.y = element_text(margin = margin(l = 5))
  )


# Percentual de Erro

ggplot(dados1, aes(x = Carga.de.usuarios, y = Percentual.de.erro, color = Arquitetura, group = Arquitetura)) +
  geom_line(size = 1.5) +  # Aumenta a espessura das linhas
  geom_point(size = 4) +  # Aumenta o tamanho dos pontos
  labs(
    x = "Carga de Usuários",
    y = "Percentual de erro") +
  theme_minimal() +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, size = 1),  # Reduz a espessura da borda
    legend.position = "bottom",  
    legend.title = element_text(size = 12, colour = "black", family = "Times"),  
    legend.text = element_text(size = 10, colour = "black", family = "Times"),  
    axis.title.x = element_text(colour = "black", family = "Times", size = 13),  # Ajusta o tamanho das fontes dos rótulos do eixo x e y
    axis.title.y = element_text(colour = "black", family = "Times", size = 13),
    axis.text.x = element_text(margin = margin(b = 5)),
    axis.text.y = element_text(margin = margin(l = 5))
  )


# Latência

ggplot(dados1, aes(x = Carga.de.usuarios, y = Latência, color = Arquitetura, group = Arquitetura)) +
  geom_line(size = 1.5) +  # Aumenta a espessura das linhas
  geom_point(size = 4) +  # Aumenta o tamanho dos pontos
  labs(
    x = "Carga de Usuários",
    y = "Latência") +
  theme_minimal() +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, size = 1),  # Reduz a espessura da borda
    legend.position = "bottom",  
    legend.title = element_text(size = 12, colour = "black", family = "Times"),  
    legend.text = element_text(size = 10, colour = "black", family = "Times"),  
    axis.title.x = element_text(colour = "black", family = "Times", size = 13),  # Ajusta o tamanho das fontes dos rótulos do eixo x e y
    axis.title.y = element_text(colour = "black", family = "Times", size = 13),
    axis.text.x = element_text(margin = margin(b = 5)),
    axis.text.y = element_text(margin = margin(l = 5))
  )


# Taxa de Transferência

ggplot(dados1, aes(x = Carga.de.usuarios, y = Taxa.de.transferência, color = Arquitetura, group = Arquitetura)) +
  geom_line(size = 1.5) +  # Aumenta a espessura das linhas
  geom_point(size = 4) +  # Aumenta o tamanho dos pontos
  labs(
    x = "Carga de Usuários",
    y = "Taxa de transferência") +
  theme_minimal() +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, size = 1),  # Reduz a espessura da borda
    legend.position = "bottom",  
    legend.title = element_text(size = 12, colour = "black", family = "Times"),  
    legend.text = element_text(size = 10, colour = "black", family = "Times"),  
    axis.title.x = element_text(colour = "black", family = "Times", size = 13),  # Ajusta o tamanho das fontes dos rótulos do eixo x e y
    axis.title.y = element_text(colour = "black", family = "Times", size = 13),
    axis.text.x = element_text(margin = margin(b = 5)),
    axis.text.y = element_text(margin = margin(l = 5))
  )

# Pacotes Recebidos

ggplot(dados1, aes(x = Carga.de.usuarios, y = Pacotes.recebidos, color = Arquitetura, group = Arquitetura)) +
  geom_line(size = 1.5) +  # Aumenta a espessura das linhas
  geom_point(size = 4) +  # Aumenta o tamanho dos pontos
  labs(
    x = "Carga de Usuários",
    y = "Pacotes recebidos") +
  theme_minimal() +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, size = 1),  # Reduz a espessura da borda
    legend.position = "bottom",  
    legend.title = element_text(size = 12, colour = "black", family = "Times"),  
    legend.text = element_text(size = 10, colour = "black", family = "Times"),  
    axis.title.x = element_text(colour = "black", family = "Times", size = 13),  # Ajusta o tamanho das fontes dos rótulos do eixo x e y
    axis.title.y = element_text(colour = "black", family = "Times", size = 13),
    axis.text.x = element_text(margin = margin(b = 5)),
    axis.text.y = element_text(margin = margin(l = 5))
  )


# Pacotes Enviados

ggplot(dados1, aes(x = Carga.de.usuarios, y = Pacotes.enviados, color = Arquitetura, group = Arquitetura)) +
  geom_line(size = 1.5) +  # Aumenta a espessura das linhas
  geom_point(size = 4) +  # Aumenta o tamanho dos pontos
  labs(
    x = "Carga de Usuários",
    y = "Pacotes enviados") +
  theme_minimal() +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, size = 1),  # Reduz a espessura da borda
    legend.position = "bottom",  
    legend.title = element_text(size = 12, colour = "black", family = "Times"),  
    legend.text = element_text(size = 10, colour = "black", family = "Times"),  
    axis.title.x = element_text(colour = "black", family = "Times", size = 13),  # Ajusta o tamanho das fontes dos rótulos do eixo x e y
    axis.title.y = element_text(colour = "black", family = "Times", size = 13),
    axis.text.x = element_text(margin = margin(b = 5)),
    axis.text.y = element_text(margin = margin(l = 5))
  )
