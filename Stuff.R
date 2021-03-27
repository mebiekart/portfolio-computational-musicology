```{r}
ggplot(corpus, aes(energy, fill = category)) +
  geom_histogram(position = "dodge", binwidth = 0.1) +
  theme_minimal() +
  labs(title = "Energy of Muscial and Opera Tracks",
       x = "Energy",
       y = "Count")
```
\
Almost all opera songs have an energy value between 0 and 0.25. However, musical songs are much more distributed and generally have more energy. \

```{r}
ggplot(corpus, aes(valence, fill = category)) +
  geom_histogram(position = "dodge", binwidth = 0.1) +
  theme_minimal() +
  labs(title = "Valence of Musical and Opera Tracks",
       x = "Valence",
       y = "Count")
```
\
It is quite surprising to me that so many opera songs have such a low valence value. This suggests that almost all opera songs in my corpus are sad. Furthermore, I expected musical songs to be even happier, i.e., to have more songs in the corpus with valence values between 0.75 and 1. \

```{r}
ggplot(corpus, aes(danceability, fill = category)) +
  geom_histogram(position = "dodge", binwidth = 0.25) +
  theme_minimal() +
  labs(title = "Danceability of Musical and Opera Tracks",
       x = "Danceability",
       y = "Count")
```
\
As expected, musical songs are more danceable than opera songs. However, I expected to find more musical songs with danceability values between 0.75 and 1.
\
```{r}
ggplot(corpus, aes(tempo, fill = category)) +
  geom_histogram(position = "dodge", binwidth = 50) +
  theme_minimal() +
  labs(title = "Tempo of Musical and Opera Tracks",
       x = "Tempo (BPM)",
       y = "Count")
```
\
Out of all features I measured, opera and musical tracks are most similar in tempo. This is surprising to me, because I would've thought musical songs have much higher tempo than opera songs.
\

### Description of the corpus

The corpus that I use for my portfolio will consist of 100 opera songs and 100 musical songs, collected from pre-existing playlists that are available on Spotify. I use Spotify's *Opera 100: Spotify Picks* playlist as a basis for my opera playlist. This playlist consists of 97 tracks, so I have added 3 tracks manually, based on Spotify's suggestions. For my musical playlist, I use a public playlist called *BROADWAY MUSICALS*, made by Hugo Torres. Out of all musical playlists I could find, I found this to be the most inclusive. Furthermore, I chose to focus on Broadway musicals because they were written to be performed in front of a live audience, just like operas. The original playlist consists of 150 songs, so I manually removed 50 tracks. I chose to remove tracks from musicals which had more tracks in the playlist, in order to create a playlist with as much different musicials as possible. \
I chose this corpus because I have always been fascinated by musicals. More recently, I was introduced to operas and I recognized the same compelling drama I appreciate in musicals. Opera songs and musical songs both mainly serve to tell a story, but have very different styles. I wonder if opera and musical music share certain aspects, because they both have such a strong narrative function. \ \

Natural comparison points: \
In comparing opera tracks with musical tracks, I expect to find a difference in tempo and danceability. Furthermore, I wonder if opera songs are sadder than musical songs, which might be reflected in the valence. I am curious to find out if the energy and loudness differ between the groups. I expect the liveness, intrumentalness, and speechiness to be similar, because most songs in the corpus are studio recordings and contain vocals. \ \

Weaknesses of the corpus: \
Because adding music from *every* opera and musical would create a very big corpus, tracks from some operas and musicals are essentially missing (also because most operas and musicals have had a lot of productions with different artists/conductors/musicians). This means that my corpus does not cover the whole genre. Furthermore, Spotify's pre-existing playlists generally include only the well-known (classical) operas and musicals, leaving out smaller productions. \ \

Typical tracks: \
* Habanera – Carmen: for me, this is a typical opera song with very high notes that everyone knows.
* La donne e mobile – Rigoletto: again, this is a very famous song. I think the grandeur of this song is typical for opera music.
* One Day More - Les Miserables: this song is very dramatic and has multiple singers, which is typical for musical songs.
* You Can't Stop The Beat - Hairspray: the happiness and danceability of this song is typical for musical songs. \ \

Atypical tracks: \
  * Summertime - Porgy and Bess: this is a jazzy song, which is a different genre than most opera songs.
  * Ride of the Valkyries - Die Walkure: this song has no lyrics, which is atypical for an opera song.
  * Totally Fucked - Spring Awakening: this comes close to a rock song, which is a different genre than most musical songs. 
  * Land of Lola - Kinky Boots: this song has a strong disco vibe, with more use of electronic instruments than the average musical song.
  
   geom_text(aes(x = energy, y = valence, label = label),
            data = tibble(label = c("Memory", "Stizzoso, mio stizzoso, voi fate il borioso"),
                          category = c("Musical Tracks", "Opera Tracks"),
                          energy = c(0.00803, 0.04950),
                          valence = c(0.1220, 0.6450),
                          danceability = c(0.261, 0.6440),
                          tempo = c(74.153, 116.443)),
            size = 3,
            hjust = "left",
            vjust = "bottom",
            nudge_x = -0.05,
            nudge_y = 0.04)
            
    
### Investigating my favourite musical song (**timbre-based self-similarity matrix**). 

```{r}
library(tidyverse)
library(spotifyr)
library(compmus)

tf <-
  get_tidy_audio_analysis("1RZ6jzlPeEaDeKYe7IJ792") %>% # Change URI.
  compmus_align(bars, segments) %>%                     # Change `bars`
  select(bars) %>%                                      #   in all three
  unnest(bars) %>%                                      #   of these lines.
  mutate(
    pitches =
      map(segments,
        compmus_summarise, pitches,
        method = "rms", norm = "euclidean"              # Change summary & norm.
      )
  ) %>%
  mutate(
    timbre =
      map(segments,
        compmus_summarise, timbre,
        method = "rms", norm = "euclidean"              # Change summary & norm.
      )
  )

tf %>%
  compmus_self_similarity(timbre, "cosine") %>% 
  ggplot(
    aes(
      x = xstart + xduration / 2,
      width = xduration,
      y = ystart + yduration / 2,
      height = yduration,
      fill = d
    )
  ) +
  geom_tile() +
  coord_fixed() +
  scale_fill_viridis_c(guide = "none") +
  theme_classic() +
  labs(x = "", y = "")
```

*** 

Here, you can see a **timbre-based self-similarity matrix for the song America (West Side Story)**. The norm used is euclidean, the distance metric used is cosine, and the summary statistic used is root mean square. The Spotify segment used is "bars". You can listen to the song [here](https://open.spotify.com/track/1RZ6jzlPeEaDeKYe7IJ792?si=FZyFmQEIQf25DEVnDXvF7g).

The first thing I noticed when looking at this SSM, is that it is a little different from the chroma-based SSM for this song. Here, instead of the clear line at the 50 seconds time mark, you can find a **clear line** right at the beginning of the song. This line indicates a unique appearance of timbre/instruments in the song. Indeed, the very beginning of the song (first 10 seconds) consists of some kind of percussion which is not strongly present in the rest of the song.

Interestingly enough, the timbre-based SSM does not really show the transition of the intro into the rest of the song (which was clearly visible in the chroma-based SSM). However, you can see that the song can be divided into **2 main parts**: 0-120 seconds and 120-300 seconds. The transition at 120 seconds *was* visible in the chroma-based SSM. Here, the "big" instruments tune in and the energy is turned up (and the big dancing scene begins). 

### What does an average opera track look like? **Chromagram** of the song "Gluck, das mir verblieb".

```{r}
tote_stadt <-
  get_tidy_audio_analysis("47xZ59XjNaGgnmWy2X1WUL") %>%
  select(segments) %>%
  unnest(segments) %>%
  select(start, duration, pitches)

tote_stadt %>%
  mutate(pitches = map(pitches, compmus_normalise, "euclidean")) %>%
  compmus_gather_chroma() %>% 
  ggplot(
    aes(
      x = start + duration / 2,
      width = duration,
      y = pitch_class,
      fill = value
    )
  ) +
  geom_tile() +
  labs(x = "Time (s)", y = NULL, fill = "Magnitude") +
  theme_minimal() +
  scale_fill_viridis_c()
```
***

Here, you see a **chomagram** of the song "Gluck, das mir verblieb" (from the opera *Die Tote Stadt*). The norm for the chroma vectors used here is **euclidean**.

**Typical tracks** \
Habanera – Carmen: for me, this is a typical opera song with very high notes that everyone knows.\
La donne e mobile – Rigoletto: again, this is a very famous song. I think the grandeur of this song is typical for opera music. \
One Day More - Les Miserables: this song is very dramatic and has multiple singers, which is typical for musical songs. \
You Can't Stop The Beat - Hairspray: the happiness and danceability of this song is typical for musical songs. \ \

**Atypical tracks** \
Summertime - Porgy and Bess: this is a jazzy song, which is a different genre than most opera songs. \
Ride of the Valkyries - Die Walkure: this song has no lyrics, which is atypical for an opera song. \
Totally Fucked - Spring Awakening: this comes close to a rock song, which is a different genre than most musical songs. \
Land of Lola - Kinky Boots: this song has a strong disco vibe, with more use of electronic instruments than the average musical song.


