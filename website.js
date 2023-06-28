function download(url) {
    const a = document.createElement('a')
    a.href = "C:\Users\cbrofft1\Desktop\Codecademy Files\Personal Project Website Build\Christian Brofft Resume+.pdf"
    a.download = url.split('/').pop()
    document.body.appendChild(a)
    a.click()
    document.body.removeChild(a)
  }