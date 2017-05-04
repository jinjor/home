package server

import (
    "html/template"
    "fmt"
    "net/http"
    "os"
)

type Info struct {
    Title string
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
    contentPath := r.URL.Query().Get("content-path")
    contentTitle := r.URL.Query().Get("content-title")
    contentDescription := r.URL.Query().Get("content-description")
    var info *Info
    if contentPath != "" && contentTitle != "" && contentDescription != "" && Exists("contents/music/" + contentPath) {
      info = &Info{Title: contentTitle, Description: contentDescription, Image: "http://world-maker.com/assets/world-maker.jpg"}
    } else {
      info = defaultInfo
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
