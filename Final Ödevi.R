ev_data <- read.csv("/Users/burakbasyildiz/Downloads/Electric_Vehicle_Population_Data.csv")
library(ggplot2)
library(dplyr)

#HİSTOGRAM GRAFİĞİ OLUŞTUR

# NA olmayan Model.Year değerlerini içeren bir veri seti oluştur
ev_data_filtered <- ev_data %>%
  filter(!is.na(Model.Year))

# Model Year'a göre elektrikli araç sayısını hesapla
ev_count_by_year <- ev_data_filtered %>%
  group_by(Model.Year) %>%
  summarise(EV_Count = n())

# Model Year'ı faktör olarak ayarla ve doğru sıralamayı sağla
ev_count_by_year$Model.Year <- factor(ev_count_by_year$Model.Year, levels = as.character(2011:2023))

# RColorBrewer paketini yükleyin
library(RColorBrewer)
# Veri setini kontrol etme ve eksik veya geçersiz verileri temizleme
ev_count_by_year <- na.omit(ev_count_by_year)  # Eksik verileri kaldır

# Manuel renk paleti tanımlama
my_colors <- c(
  "2010" = "#1b9e77",
  "2011" = "#d95f02",
  "2012" = "#7570b3",
  "2013" = "#e7298a",
  "2014" = "#66a61e",
  "2015" = "#e6ab02",
  "2016" = "#a6761d",
  "2017" = "#666666",
  "2018" = "#8c564b",
  "2019" = "#2ca02c",
  "2020" = "#ff7f0e",
  "2021" = "#1f77b4",
  "2022" = "#9467bd",
  "2023" = "#d62728"  # 2023 için daha belirgin bir renk
)

# Histogram oluştur
hist_plot1 <- ggplot(ev_count_by_year, aes(x = Model.Year, y = EV_Count, fill = as.factor(Model.Year))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = EV_Count), vjust = -0.5, size = 3, color = "black") +  # Değer etiketleri ekleyin
  labs(title = "Yıllara Göre Kaydedilen Elektrikli Araç Sayısı",
       x = "Model Yılı",
       y = "Elektrikli Araç Sayısı") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1)) +  # x eksenindeki metinleri 90 derece döndür
  guides(fill = FALSE) +  # Legend'u kaldır
  scale_fill_manual(values = my_colors)  # Manuel renk paletini kullan

# Grafiği göster
print(hist_plot1)




#BUBBLE CHART
library(ggplot2)
library(dplyr)

# Elektrikli araç türleri ve markalarına göre ortalama menzilleri hesapla
mean_range_by_make <- ev_data %>%
  group_by(Make) %>%
  summarise(mean_range = mean(ifelse(Electric.Range == 0, NA, Electric.Range), na.rm = TRUE)) %>%
  filter(!is.na(mean_range)) %>%  # NA değerleri filtrele
  arrange(desc(mean_range))  # Ortalama menzile göre büyükten küçüğe sırala

# Görselleştirme: Bubble chart (pastel renkler, büyük legend)
bubble_plot <- ggplot(mean_range_by_make, aes(x = mean_range, y = reorder(Make, mean_range), color = mean_range, size = mean_range)) +
  geom_point(alpha = 0.9) +
  labs(title = "Markalara Göre Ortalama Elektrikli Menzil",
       x = "Ortalama Elektrikli Menzil (Miles)",
       y = "Marka",
       color = "Ortalama Elektrikli Menzil (Mil)") +
  scale_color_gradient(low = "blue", high = "green") +  # Pastel renk paleti
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.key.size = unit(3, "lines")) +  # Legend anahtar boyutunu ayarla
  guides(size = "none")

# Grafiği göster
print(bubble_plot)



#VIOLIN PLOT
library(ggplot2)
ev_data_clean <- ev_data[ev_data$Electric.Range != 0, ]

# Temizlenmiş veri ile violin plot oluştur
violin_plot_clean <- ggplot(ev_data_clean, aes(x = Electric.Vehicle.Type, y = Electric.Range, fill = Electric.Vehicle.Type)) +
  geom_violin(trim = FALSE) +
  labs(title = "Elektrikli Araç Türlerine Göre Elektrikli Menzil Dağılımı",
       y = "Menzil",
       fill = "Elektrikli Araç Türü") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), # x-eksenindeki metinleri kaldır
        axis.ticks.x = element_blank(), # x-eksenindeki işaretleri kaldır
        legend.position = "bottom") + # leyendayı aşağıya taşı
  xlab(NULL) # x-ekseni etiketini kaldır

# Temizlenmiş grafiği görüntüle
print(violin_plot_clean)



#SÜTUN GRAFİĞİ OLUŞTURMA


library(dplyr)
library(ggplot2)

# Top 15 markayı seçme
top_15_makes <- ev_data %>%
  group_by(Make) %>%
  summarise(Count = n()) %>%
  top_n(15, Count) %>%
  arrange(desc(Count))

# Marka ile uygunluk durumunu ilişkilendirme ve sayma
brand_eligibility <- ev_data %>%
  filter(Make %in% top_15_makes$Make) %>%
  group_by(Make, Clean.Alternative.Fuel.Vehicle..CAFV..Eligibility) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%  # Sütunları büyükten küçüğe sıralama
  top_n(15, Count)  # İlk 15 markayı seçme

# Yatay çubuk grafik oluşturma (markaların yazılışını 90 derece döndürme)
bar_plot <- ggplot(brand_eligibility, aes(x = reorder(Make, -Count), y = Count, fill = Clean.Alternative.Fuel.Vehicle..CAFV..Eligibility)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +  # Genişliği ayarlama
  labs(title = "En Fazla Araç Sayısına Sahip 15 Markanın Temiz ve Alternatif Yakıtlı Araç Teşvikine Uygunluğu",
       x = "Marka",
       y = "Sayı",
       fill = "Uygunluk Durumu") +
  theme(axis.text.x = element_text(angle = 25, hjust = 1)) +  # Markaların yazılışını 90 derece döndürme
  scale_fill_manual(values = c("green", "blue", "red")) +  # Uygunluk durumu için renkler
  guides(fill = guide_legend(title = "Uygunluk Durumu")) +
  theme(legend.position = "bottom", # Uygunluk durumu için renklerin gösterimi
        panel.background = element_rect(fill = "white")) # Arka tema beyaz olsun

print(bar_plot)


# Grafiği görüntüleme
print(bar_plot)

























  






