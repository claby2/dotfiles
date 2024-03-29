// ==UserScript==
// @name         Auto Skip YouTube Ads
// @version      1.0.0
// @description  Speed up and skip YouTube ads automatically
// @match        *://*.youtube.com/*
// @exclude      *://*.youtube.com/subscribe_embed?*
// ==/UserScript==

let main = new MutationObserver(() => {
  // Fetch skip button and click it.
  let btn = document
    .getElementsByClassName("ytp-ad-skip-button ytp-button")
    .item(0);
  if (btn) {
    btn.click();
  }

  // Skip button does not exist.
  const ad = [...document.querySelectorAll(".ad-showing")][0];
  if (ad) {
    const video = document.querySelector("video");
    video.currentTime = video.duration;
  }
});

main.observe(
  document.getElementsByClassName("video-ads ytp-ad-module").item(0),
  { attributes: true, characterData: true, childList: true }
);
