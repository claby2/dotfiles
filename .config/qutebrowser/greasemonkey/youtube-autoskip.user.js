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
    // Speed up and mute ad.
    document.querySelector("video").playbackRate = 16;
    document.querySelector("video").muted = true;
  }
});

main.observe(
  document.getElementsByClassName("video-ads ytp-ad-module").item(0),
  { attributes: true, characterData: true, childList: true }
);
