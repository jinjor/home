package server

import (
    "html/template"
    "fmt"
    "net/http"
    "os"
    "encoding/json"
    "io/ioutil"
)

type Music struct {
    Id string `json:"id"`
    Path string `json:"path"`
    Title string `json:"title"`
    Description string `json:"description"`
}

type Info struct {
    Title string
    Path string
    Description string
    Image string
}

func init() {
    http.HandleFunc("/", handler)
}

var defaultInfo =
  &Info{
    Title: "World Maker",
    Description: "ジンジャーと Yosuke Torii のホームページ",
    Image: "http://world-maker.com/assets/world-maker.jpg",
  }


func handler(w http.ResponseWriter, r *http.Request) {
    contentId := r.URL.Query().Get("content")
    info := defaultInfo
    if contentId != "" {
      file, _ := ioutil.ReadFile("contents/music.json")
      musics := make([]Music,0)
      json.Unmarshal(file, &musics)
      musicMap := make(map[string]Music)
      for _, value := range musics {
        musicMap[value.Id] = value
      }
      if _, ok := musicMap[contentId]; ok {
        music := musicMap[contentId]
        if Exists("contents/music/" + music.Path) {
          info = &Info{
            Title: music.Title,
            Description: music.Description,
            Image: "http://world-maker.com/assets/world-maker.jpg",
          }
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

func Exists(filename string) bool {
	_, err := os.Stat(filename)
	return err == nil
}
