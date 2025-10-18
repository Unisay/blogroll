{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Blogroll.Html where

import Blogroll.Feed (mergeFeedEntries, parseFeed)
import Blogroll.Fetch (extractDomain, fetchAllFavicons, fetchFeed)
import Blogroll.Type (FeedEntry (..), Blogroll (..))
import Control.Concurrent.Async (mapConcurrently)
import Data.List (groupBy)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time (defaultTimeLocale, formatTime)

generateDomainCssClass :: Text -> Text
generateDomainCssClass domain =
  "favicon-" <> T.map (\c -> if c `elem` ['.', '-'] then '_' else c) domain

generateFaviconCss :: Map.Map Text Text -> Text
generateFaviconCss faviconMap =
  T.concat $ map generateSingleCss (Map.toList faviconMap)
  where
    generateSingleCss (domain, base64) =
      T.concat
        [ ".",
          generateDomainCssClass domain,
          "::after {",
          "content: ''; ",
          "display: inline-block; ",
          "width: 16px; ",
          "height: 16px; ",
          "margin-left: 8px; ",
          "background-image: url(data:image/png;base64,",
          base64,
          "); ",
          "background-size: contain; ",
          "background-repeat: no-repeat; ",
          "vertical-align: middle; ",
          "}\n"
        ]


renderAll :: Blogroll -> IO ()
renderAll blogroll = do
  let urls = blogroll.urls
  faviconMap <- fetchAllFavicons urls
  let faviconCss = generateFaviconCss faviconMap
  feeds <- mapConcurrently fetchFeed urls
  let feedEntries =
        zipWith
          ( \url result -> case result of
              Left _err -> []
              Right cont -> parseFeed url cont
          )
          urls
          feeds

  let allEntries = mergeFeedEntries feedEntries
  putStrLn $ "Total entries: " ++ show (length allEntries)

  let recent25 = take 25 allEntries
  let recentHtml = renderHtml recent25 blogroll.title faviconCss
  let allHtml = renderHtml allEntries (blogroll.title <> " - All Posts") faviconCss

  TIO.writeFile "index.html" recentHtml
  TIO.writeFile "all.html" allHtml
  putStrLn "Generated index.html (25 recent) and all.html"

renderHtml :: [FeedEntry] -> Text -> Text -> Text
renderHtml entries title faviconCss =
  let groupedEntries = groupBy (\a b -> formatDate a.entryDate == formatDate b.entryDate) entries
      entriesHtml = T.concat $ map renderDateGroup groupedEntries
   in """
      <!DOCTYPE html>
      <html>
      <head>
      <meta name="viewport" content="width=device-width, initial-scale=1.0">
      <title>RSS Reader</title>
      <style>
           body {
             font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, sans-serif;
             max-width: 800px;
             margin: 0 auto;
             padding: 1em;
             background-color: #1F1F1F;
             color: #CCCCCC;
             display: flex;
             flex-flow: column;
             align-content: center;
           }
           h1 {
             color: #CCCCCC;
             border-bottom: 2px solid #0078D4;
             padding-bottom: 10px;
           }
           h2.date-header {
             color: #6E7681;
             font-size: 0.9em;
             font-weight: 600;
             margin-top: 1.5em;
             margin-bottom: 0.5em;
             padding-left: 8px;
           }
           ul {
             list-style: none;
             padding: 0;
             margin-top: 0;
             margin-bottom: 0;
           }
           li {
             padding-left: 8px;
             padding-bottom: 8px;
           }
           a {
             color: #0078D4;
             text-decoration: none;
             font-weight: 500;
           }
           a:visited {
             color: #569CD6;
           }
           a:hover {
             text-decoration: underline;
             color: #3CA3FF;
           }
           .source {
             color: #6E7681;
             font-size: 0.8em;
             padding-left: 0.5em;
           }
           """
        <> faviconCss
        <> """
           </style>
           </head>
            <body>
             <h1>"""
        <> title
        <> """</h1>
           """
        <> entriesHtml
        <> """
              <ul><li><a href=\"all.html\">See all</a></li></ul>
            </body>
           </html>
           """
  where
    formatDate = T.pack . formatTime defaultTimeLocale "%Y-%m-%d"

    renderDateGroup :: [FeedEntry] -> Text
    renderDateGroup [] = ""
    renderDateGroup (entry : rest) =
      T.concat
        [ "<h2 class=\"date-header\">",
          formatDate entry.entryDate,
          "</h2>\n<ul>\n",
          T.concat $ map renderEntry (entry : rest),
          "</ul>\n"
        ]

    renderEntry entry =
      T.concat
        [ "<li><a href=\"",
          entryLink entry,
          "\" class=\"",
          generateDomainCssClass (extractDomain entry.entrySiteUrl),
          "\">",
          entryTitle entry,
          "</a><span class=\"source\">(",
          extractDomain entry.entrySiteUrl,
          ")</span></li>\n"
        ]
