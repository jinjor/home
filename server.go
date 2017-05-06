package server

import (
    "html/template"
    "fmt"
    "net/http"
    "encoding/json"
    "io/ioutil"
)

type Music struct {
    Id string `json:"id"`
    Title string `json:"title"`
    Description string `json:"description"`
    Image string `json:"image"`
}

type Info struct {
    Title string
    Description string
    Image string
    Card string
    PlayerUrl string
}

func init() {
    http.HandleFunc("/", handler)
}

var defaultInfo =
  &Info{
    Title: "World Maker",
    Description: "ジンジャーと Yosuke Torii のホームページ",
    Image: "http://world-maker.com/assets/world-maker.jpg",
    Card: "summary",
    PlayerUrl: "",
  }


func handler(w http.ResponseWriter, r *http.Request) {
    contentId := r.URL.Query().Get("content")
    info := defaultInfo
    if contentId != "" {
      file, _ := ioutil.ReadFile("music.json")
      musics := make([]Music,0)
      json.Unmarshal(file, &musics)
      musicMap := make(map[string]Music)
      for _, value := range musics {
        musicMap[value.Id] = value
      }
      if _, ok := musicMap[contentId]; ok {
        music := musicMap[contentId]
        image := "http://world-maker.com/assets/world-maker.jpg"
        if music.Image != "" {
          image = "http://world-maker.com/contents/music/jacket/" + music.Image
        }
        playerUrl := "https://world-maker.appspot.com/player?content=" + contentId
        info = &Info{
          Title: music.Title,
          Description: music.Description,
          Image: image,
          Card: "player",
          PlayerUrl: playerUrl,
        }
      }
    }
    t, err := template.ParseFiles("index.html")
    if err != nil {
      fmt.Fprintf(w, "%v\n", err)
      return
    }
    t.Execute(w, info)
}
