Alsatian drama: character speech & social positions
================
Artjoms Šeļa
2023-08-17

- <a href="#setup" id="toc-setup">Setup</a>
- <a href="#data-processing" id="toc-data-processing">Data processing</a>
- <a href="#exploration-metadata"
  id="toc-exploration-metadata">Exploration: metadata</a>
  - <a href="#female-characters" id="toc-female-characters">Female
    characters</a>
  - <a href="#male-characters" id="toc-male-characters">Male characters</a>
- <a href="#exploration-keywords-under-curves"
  id="toc-exploration-keywords-under-curves">Exploration: Keywords under
  curves</a>
  - <a href="#distinctiveness-and-size"
    id="toc-distinctiveness-and-size">Distinctiveness and size</a>
  - <a href="#top-10-minor-characters" id="toc-top-10-minor-characters">Top
    10 minor characters</a>
  - <a href="#top-5-average-characters"
    id="toc-top-5-average-characters">Top 5 average characters</a>
  - <a href="#3-most-distinctive-large-roles-35-50"
    id="toc-3-most-distinctive-large-roles-35-50">3 most distinctive large
    roles (35-50%)</a>
  - <a href="#2-most-distinctive-extreme-roles-50-65"
    id="toc-2-most-distinctive-extreme-roles-50-65">2 most distinctive
    extreme roles (50-65%)</a>

## Setup

``` r
## main
library(tidyverse)
library(tidytext)
library(tidylo) # log odds library

## visualisations
library(ggsankey)
library(patchwork)
library(grid)

## functions for 'curves-under-keywords' method
source("src/loo_distinct.R")
```

## Data processing

``` r
## read data
als <- read_tsv("https://git.unistra.fr/methal/alsatian-character-speech/-/raw/main/overall-per-character-speech-postpro.tsv")

## make sure characters have unique IDs
ids <- als %>% 
  reframe(speaker_id = paste(speaker,play_short_name)) %>% 
  distinct(speaker_id,.keep_all = T) %>% 
  mutate(char_id = paste0(str_extract(speaker_id,"^."),row_number()))

## cobmine IDs with the main table
als <-  als %>% 
  mutate(speaker_id = paste(speaker,play_short_name)) %>% 
  left_join(ids,by="speaker_id")
```

Here I collapse all utterances per character into bag of words (one
character per row) and tokenize it; separate metadata table will serve
for referencing later. [Better
tokenizer](https://zenodo.org/records/2454993) exists for Alsatian, but
I don’t have time to implement it right now.

``` r
als_speech <- als %>% group_by(char_id,play_short_name) %>% summarize(text=paste(text,collapse="\n\n"))

als_meta <- als %>% select(-text) %>% distinct(char_id,.keep_all = T)

als_tok <- als_speech %>%
  group_by(char_id,play_short_name) %>% 
  unnest_tokens(input=text,output="word",token="words")
```

I also infer author’s gender based on the first name (which might be bad
idea)

``` r
author_g <- als_meta %>% 
  count(author,sort = T) %>% 
  select(-n) %>% 
  mutate(g_author = c("M", "M", "M", "M","M", "F","M","M","M","M","M", "M","M","M","F","M","M","F","M", "M","M","F","M","M"))
```

Counting: how much words each character says and what proportion does it
make of the whole play.

``` r
counts <- als_tok %>%
  group_by(char_id,play_short_name) %>%
  count(char_id) %>%  # count rows/words
  group_by(play_short_name) %>%
  mutate(char_part = n/sum(n)) %>% # proportion to the whole
  select(-play_short_name)

t<-counts %>% 
  arrange(-n,-char_part) %>%
  head(10)
knitr::kable(t)
```

| play_short_name               | char_id |    n | char_part |
|:------------------------------|:--------|-----:|----------:|
| gunther-dr-cousin-refractaire | G345    | 7044 | 0.5837892 |
| stoskopf-wenn-d-fraue-wahle   | E648    | 7015 | 0.3644534 |
| greber-d-jumpfer-prinzesse    | J290    | 6885 | 0.3069276 |
| stoskopf-dr-herr-maire        | D565    | 5800 | 0.3991467 |
| stoskopf-dr-hoflieferant      | F602    | 5492 | 0.3704053 |
| arnold-der-pfingstmontag      | L9      | 4899 | 0.1472631 |
| arnold-der-pfingstmontag      | H11     | 4493 | 0.1350588 |
| stoskopf-ins-ropfers-apothek  | A630    | 4280 | 0.2826202 |
| weber-e-bissel-lieb           | J696    | 4163 | 0.4160088 |
| arnold-der-pfingstmontag      | F4      | 4076 | 0.1225238 |

Combining information on characters & authors back to the metadata

``` r
als_meta<-als_meta %>% left_join(author_g,by="author") %>%
  left_join(counts)
```

## Exploration: metadata

I wanted to do quick Sankey plots to see how characters relate to
categories. First, all together.

``` r
df <- als_meta %>%
  filter(!str_detect(gender, "gdr_err"),
#         job_category != "jact_err",
         !gender %in% c("U", "B")) %>% 
  make_long(social_class, gender,job_category,genre) 

ggplot(df, aes(x = x, 
               next_x = next_x, 
               node = node, 
               next_node = next_node,
               fill = factor(node),
               label = node)) +
  geom_sankey(flow.alpha = .6,node.color = "gray30") +
  geom_sankey_label(size = 3, color = "white", fill = "gray40") +
  scale_fill_viridis_d() +
  theme_sankey(base_size = 18) +
  labs(x = NULL) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = .5)) +
  ggtitle("Character distribution: all")
```

![](alsatian_characters_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->
Now, what if we look only at characters that speak at least 1000 words?

``` r
df <- als_meta %>%
  filter(!str_detect(gender, "gdr_err"),
         !gender %in% c("U", "B"),
         n >= 1000) %>% 
  make_long(social_class, gender,job_category,genre) 

ggplot(df, aes(x = x, 
               next_x = next_x, 
               node = node, 
               next_node = next_node,
               fill = factor(node),
               label = node)) +
  geom_sankey(flow.alpha = .6,
              node.color = "gray30") +
  geom_sankey_label(size = 3, color = "white", fill = "gray40") +
  scale_fill_viridis_d() +
  theme_sankey(base_size = 18) +
  labs(x = NULL) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = .5)) +
  ggtitle("Character distribution: major speakers")
```

![](alsatian_characters_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

Difference is quite striking: major female characters become even less
working and disappear from “elementary professions”, which suggests it’s
the space for coding minor ‘servants’. Men, on the other hand,
proportionally increase in employement compared to total distribution.

Major female characters now have less diversity in social coding, and it
is a clear split between the lowest and the highest class. Does that
imply they are ‘love interests’/‘brides’? Also, they are basically
allowed to have professions only in comedies (see below).

Below I zoom into characters of each gender separately.

### Female characters

``` r
df <- als_meta %>%
  filter(!str_detect(gender, "gdr_err"),
         #!str_detect(job_category, "gdr_err"),
         !gender %in% c("U", "B"),
         gender=="F",
         n>1000) %>% 
  make_long(social_class, gender, genre,job_category) 

ggplot(df, aes(x = x, 
               next_x = next_x, 
               node = node, 
               next_node = next_node,
               fill = factor(node),
               label = node)) +
  geom_sankey(flow.alpha = .6,
              node.color = "gray30") +
  geom_sankey_label(size = 3, color = "white", fill = "gray40") +
  scale_fill_viridis_d() +
  theme_sankey(base_size = 18) +
  labs(x = NULL) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = .5)) +
  ggtitle("Major female character distribution")
```

![](alsatian_characters_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

### Male characters

``` r
df <- als_meta %>%
  filter(!str_detect(gender, "gdr_err"),
         #!str_detect(job_category, "err$"),
         !gender %in% c("U", "B"),
         gender=="M",
         n>=1000) %>% 
  make_long(social_class, gender, genre,job_category) 

ggplot(df, aes(x = x, 
               next_x = next_x, 
               node = node, 
               next_node = next_node,
               fill = factor(node),
               label = node)) +
  geom_sankey(flow.alpha = .6,
              node.color = "gray30") +
  geom_sankey_label(size = 3, color = "white", fill = "gray40") +
  scale_fill_viridis_d() +
  theme_sankey(base_size = 18) +
  labs(x = NULL) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = .5)) +
  ggtitle("Character distribution")
```

![](alsatian_characters_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

## Exploration: Keywords under curves

Here I use a simplistic ‘distinctiveness’ measure based on words for
general sanity checks etc. We use it only for exploratory reasons: words
are easy to read, but generally it’s bad idea to use it for further
inference, because lexis is not good at capturing language variation
(inflections, dialect renderings etc.). Plus it’s hard to bootstrap
keywords.

See full procedure with examples [in this
notebook](https://github.com/perechen/difs-character-voices/blob/master/03_analysis.md).
Basically, we calculate N top keywords per character (as compared
vs. the rest of the play), order them and measure the area under the
resulting curve. The more area there is – the more ‘distinctive’ a
character appears.

I use here 50 keywords per character, and only characters that do have
at least 1000 words spoken (it’s a very low bar tbh)

First, setup.

``` r
## variable setup
n_size <- 1000 # how many words a character needs to have to be analysed
n_char <- 1 # at least how many characters
feature_bag=50 # how many keywords per character to consider

## plays that have at least 1 target character
texts <- als_meta %>%
  filter(n>n_size) %>%
  count(play_short_name) %>%
  filter(n>=n_char) %>% 
  pull(play_short_name)

## mark characters that speak enough for analysis
target_char <-  als_tok %>%
  count(play_short_name,char_id) %>%
  mutate(isTarget = TRUE)

## add marking to main table
char_fin <- als_tok %>%
  filter(play_short_name %in% texts) %>%
  left_join(target_char %>% ungroup() %>% select(char_id, isTarget),by="char_id")

head(char_fin,10)
```

    ## # A tibble: 10 × 4
    ## # Groups:   char_id, play_short_name [1]
    ##    char_id play_short_name      word    isTarget
    ##    <chr>   <chr>                <chr>   <lgl>   
    ##  1 '265    fuchs-heimlichi-lieb mr      TRUE    
    ##  2 '265    fuchs-heimlichi-lieb gehn    TRUE    
    ##  3 '265    fuchs-heimlichi-lieb grad    TRUE    
    ##  4 '265    fuchs-heimlichi-lieb durich  TRUE    
    ##  5 '265    fuchs-heimlichi-lieb d       TRUE    
    ##  6 '265    fuchs-heimlichi-lieb kiche   TRUE    
    ##  7 '265    fuchs-heimlichi-lieb fritzel TRUE    
    ##  8 '265    fuchs-heimlichi-lieb mr      TRUE    
    ##  9 '265    fuchs-heimlichi-lieb sin     TRUE    
    ## 10 '265    fuchs-heimlichi-lieb jo      TRUE

Now, analysis loop.

``` r
df_res <- NULL # for saving results

for(dr in texts) {
  message(paste0("----",dr))
  ## for each drama
  
  ## 1. 'reference corpus' pool (here, it means one drama)
  pool <- char_fin %>% filter(play_short_name==dr)
  n_char <- pool$char_id %>% unique() %>% length()
  
  ## 1.1. if it's a monologue drama, then skip
  if(n_char <= 1) {
    next
  }
  
  ## 2. take all characters that are 'targets'
  chr_pool <-  target_char %>% filter(play_short_name==dr)
  
  ## here loop for character should start
  for(c in 1:nrow(chr_pool)) {
    message(paste0("--",chr_pool$char_id[c]))
    ## 3. for each 'target' character
    
    ## 4. calculate log-odds, take the top ones.
    ## this uses convenience function from 'src/loo_distinct.csv'
    top_curve <- logodds_curve(pool,
                               char=chr_pool$char_id[c],
                               features=feature_bag,
                               oversampling = T)
    
    ## 5. calculate area under the curve
    auc <- calculate_auc(top_curve)
    
    ## combine into a dataframe
    df <- top_curve %>%
      mutate(d=auc,
             play_short_name=dr,
             n=chr_pool$n[c])
    
    ## combine with results
    df_res <- bind_rows(df_res,df)
    
    
    
  } ## end of character loop
  
} ## end of text loop

write_tsv(df_res,file="data/logodds_curves.tsv")
```

### Distinctiveness and size

All distinctiveness measures are sensitive to the size of target
characters vs. reference corpus. The smaller the character, the more
‘distinct’ it is, because frequency distributions of words become more
wild and unstable the less data we have. See on the plot below.

    ## png 
    ##   2

![](img/part_d.png)

This means it would be wise to be interested not in the absolute
distinctive characters, but distinctive characters relative to their
part in the play We can cut data into ‘percentile’ bins based on the
overall distribution of `d` measure.

``` r
## determine data 'bins' based on percentiles
cuts <- df_res$char_part %>% cut(breaks=4)

## add this data
df_res<-df_res %>% ungroup() %>%  mutate(part_bin = as.integer(cuts)) 

## show how these bins are distributed in data
df_res %>% ggplot(aes(char_part,d,color=as.factor(part_bin))) + geom_point() + theme_minimal() + scale_color_viridis_d(option="C")
```

![](alsatian_characters_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

Roughly, it breaks our characters in four groups: minor, average, major,
extreme. We can at look at top distinctive characters in each.

### Top 10 minor characters

``` r
### 10 most distinctive smaller roles (5-12%)
c1<-df_res %>% arrange(part_bin,-d) %>%
  filter(part_bin==1) %>% 
  arrange(-d) %>% 
  top_n(10,d) %>% 
  select(d,speaker,gender,social_class,job_category,part_bin,play_short_name,keywords)
knitr::kable(c1)
```

|        d | speaker          | gender | social_class       | job_category                       | part_bin | play_short_name             | keywords                                           |
|---------:|:-----------------|:-------|:-------------------|:-----------------------------------|---------:|:----------------------------|:---------------------------------------------------|
| 312.2145 | Gläsler          | M      | upper_middle_class | service_and_sales                  |        1 | arnold-der-pfingstmontag    | schwyge, dee, ae, lustigg, no                      |
| 258.2345 | Reinhold         | M      | upper_class        | professionals_scientific_technical |        1 | arnold-der-pfingstmontag    | konnt, sonst, wahrlich, wunsch, zieh’n             |
| 254.7610 | Wolfgang         | M      | NA                 | NA                                 |        1 | arnold-der-pfingstmontag    | gewogen, liebe, umgang, mein, froher               |
| 227.3318 | Dr. Freundlich   | M      | upper_middle_class | professionals_scientific_technical |        1 | stoskopf-dr-herr-maire      | lebensgefährtin, unterdessen, volkslied, den, eine |
| 197.9783 | Friedmann        | M      | lower_class        | service_and_sales                  |        1 | greber-s-teschtament        | daran, einen, beide, mark, bei                     |
| 193.4669 | Hans Grinsinger  | M      | upper_class        | industry_and_transportation        |        1 | stoskopf-dr-hoflieferant    | mal, attacke, ch, herbei, mobil                    |
| 192.8513 | Frau Rosine      | F      | NA                 | NA                                 |        1 | arnold-der-pfingstmontag    | gsengelt, kain’s, kauft, mann’s, nodelehr          |
| 192.1985 | Barbara (Berwel) | F      | NA                 | NA                                 |        1 | arnold-der-pfingstmontag    | knollfink, leßt, mollekopf, sinner, stattli’s      |
| 190.9161 | Karl Gauthier    | M      | middle_class       | government_executive_officials     |        1 | stoskopf-dr-hoflieferant    | lebt, pech, tranken, vereinsausflug, wird’s        |
| 183.1589 | Schampetiss      | M      | lower_class        | elementary_professions             |        1 | stoskopf-wenn-d-fraue-wahle | valentin, weltg’schicht, wiebsbild, kann, bisl     |

### Top 5 average characters

``` r
### 5 most distinctive average roles (12-35%)
c2 <- df_res %>% arrange(part_bin,-d) %>%
  filter(part_bin==2) %>% 
  arrange(-d) %>% 
  top_n(5,d) %>% 
  select(d,speaker,gender,social_class,job_category,part_bin,play_short_name,keywords)

knitr::kable(c2)
```

|        d | speaker            | gender | social_class       | job_category                       | part_bin | play_short_name                           | keywords                                        |
|---------:|:-------------------|:-------|:-------------------|:-----------------------------------|---------:|:------------------------------------------|:------------------------------------------------|
| 134.3575 | Abbé Mülberg       | M      | lower_middle_class | clergy                             |        2 | greber-lucie                              | der, du, in, es, ist                            |
| 129.2009 | Otto Kühne         | M      | upper_class        | professionals_scientific_technical |        2 | schneegans-steckelburjer-universite-saaue | auf, dafür, echten, gehabt, gelungen            |
| 114.6617 | ’s Stawalders Jean | M      | NA                 | NA                                 |        2 | fuchs-heimlichi-lieb                      | wald, waue, herz, gredel, in                    |
| 107.3632 | Hans               | M      | upper_middle_class | agriculture                        |        2 | bastian-dr-hans-im-schnokeloch            | riewe, risst, s’münster, streifgärnel, verbiete |
| 106.6197 | Désiré Enishänsel  | M      | upper_middle_class | professionals_scientific_technical |        2 | greber-sainte-cecile                      | verstehn, veryffer, weh, üsschnüüfe, aase       |

### 3 most distinctive large roles (35-50%)

``` r
c3<-df_res %>% arrange(part_bin,-d) %>%
  filter(part_bin==3) %>% 
  arrange(-d) %>% 
  top_n(3,d) %>% 
  select(d,speaker,gender,social_class,job_category,part_bin,play_short_name,keywords)

knitr::kable(c3)
```

|        d | speaker          | gender | social_class       | job_category                   | part_bin | play_short_name             | keywords                                      |
|---------:|:-----------------|:-------|:-------------------|:-------------------------------|---------:|:----------------------------|:----------------------------------------------|
| 97.58072 | Fritz Grinsinger | M      | upper_class        | industry_and_transportation    |        3 | stoskopf-dr-hoflieferant    | spiel, zueluejt, üewwermorje, isch, kummt     |
| 94.85432 | D’r Herr Maire   | M      | upper_middle_class | government_executive_officials |        3 | stoskopf-dr-herr-maire      | r, aktie, angelejeheit, arrièrecousin, d’äuje |
| 89.40322 | Emile Schwartler | M      | lower_middle_class | rentiers                       |        3 | stoskopf-wenn-d-fraue-wahle | anno, appetit, artikele, betrifft, bonne      |

### 2 most distinctive extreme roles (50-65%)

``` r
c4<-df_res %>% arrange(part_bin,-d) %>%
  filter(part_bin==4) %>% 
  arrange(-d) %>% 
  top_n(2,d) %>% 
  select(d,speaker,gender,social_class,job_category,part_bin,play_short_name,keywords)

knitr::kable(c4)
```

|        d | speaker              | gender | social_class       | job_category           | part_bin | play_short_name               | keywords                                    |
|---------:|:---------------------|:-------|:-------------------|:-----------------------|---------:|:------------------------------|:--------------------------------------------|
| 69.69423 | Gottfried Nussbicker | M      | lower_middle_class | service_and_sales      |        4 | gunther-dr-cousin-refractaire | deutsch, dies, dire, däj, d’antwort         |
| 64.63097 | Marie                | F      | lower_class        | elementary_professions |        4 | riff-s-paradies               | garte, geläjeheit, genuej, genüej, gewäsche |

\`\`\`
