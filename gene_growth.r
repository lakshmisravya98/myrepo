#Downdoing file
url = "https://raw.githubusercontent.com//markziemann//SLE712_files//master//bioinfo_asst3_part1_files//gene_expression.tsv"
destfile = "gene_expression.tsv"
download.file(url, destfile)

#loading data into R obejct
library(dplyr)

gene_expression = read.table(file= "gene_expression.tsv", sep = '\t',fill = TRUE, header = TRUE)  # Reading data using read.table function
View(gene_expression)

 
#1)Read in the file, making the gene accession numbers the row names. Show a table of values for the
#first six genes.

head(gene_expression,6)     # displaying first 6 rows 

#2)Make a new column which is the mean of the other columns. Show a table of values for the first six
#genes.
gene_expression$Mean = (gene_expression$SRR5150592+gene_expression$SRR5150593)/2     # calculating mean of both columns
head(gene_expression)

#3)List the 10 genes with the highest mean expression
Max_10_mean= top_n(gene_expression, 10)                      # top 10 max mean values
View(Max_10_mean)

 
#4)Determine the number of genes with a mean <10
 library(dplyr)
 with_mean_less_than10 = gene_expression %>%
                         filter(gene_expression$Mean < 10 )   
 print( with_mean_less_than10)

#5)Make a histogram plot of the mean values in png format and paste it into your report
 mean_hist=hist(gene_expression$Mean)
 png("mean_hist.png")


#-------------------------------------------------------------------------------------------------------------
 #downloading files from give url
 url = "https://raw.githubusercontent.com//markziemann//SLE712_files//master//bioinfo_asst3_part1_files//growth_data.csv"
 destfile = "growth_data.csv"
 download.file(url, destfile)
 
#6. Import this csv file into an R object. What are the column names?
 
 growth_data = read.csv("growth_data.csv", stringsAsFactors = FALSE)
 View(growth_data)

 column_names = names(growth_data)

#7)Calculate the mean and standard deviation of tree circumference at the start and end of the study at
 #both sites. 
 
 Mean_SD = growth_data%>%
           group_by(Site)%>%
           summarise(mean_start_year = mean(Circumf_2004_cm), sd_start_year = sd(Circumf_2004_cm),mean_end_year = mean(Circumf_2019_cm), sd_end_year = sd(Circumf_2019_cm))
   glimpse(Mean_SD)
   
#8) Make a box plot of tree circumference at the start and end of the study at both sites

northeast = growth_data%>%
            filter(Site == "northeast")
southwest = growth_data%>%
            filter(Site == "southwest")

boxplot(northeast$Circumf_2004_cm,northeast$Circumf_2019_cm, main = "Boxplot for Northeast ",names = c("start_study", "end_study"))

boxplot(southwest$Circumf_2004_cm,southwest$Circumf_2019_cm, main = "Boxplot for Southwest ",names = c("start_study", "end_study"))      

#9)Calculate the mean growth over the past 10 years at each site
mean_growth_over_10yrs = growth_data%>%
                         group_by(Site)%>%
                         summarise(mean_2019 = mean(Circumf_2019_cm), mean_2009 = mean(Circumf_2009_cm),mean_growth = mean_2019 - mean_2009)
print(mean_growth_over_10yrs)

#10)Use the t.test and wilcox.test functions to estimate the p-value that the 10 year growth is different at
#the two sites.
#for northeast site
t.test(northeast$Circumf_2009_cm,northeast$Circumf_2019_cm)
wilcox.test(northeast$Circumf_2009_cm,northeast$Circumf_2019_cm, alternative = "two.sided")

#for Southwest site
t.test(southwest$Circumf_2009_cm,southwest$Circumf_2019_cm)
wilcox.test(southwest$Circumf_2009_cm,southwest$Circumf_2019_cm, alternative = "two.sided")


#x,y: numeric vectors
#alternative: the alternative hypothesis. Allowed value is one of "two.sided" (default), "greater" or "less".

#----------------------------------------------------------------------------------------------------------------------------------

