module.exports = {
  content: ['src/**/*.elm'],
  css: ['public/dist/style.css'],
  extractors: [
    {
      extractor: content => content.match(/[A-Za-z0-9-_:\/]+/g) || [],
      extensions: ['elm']
    }
  ]
}
