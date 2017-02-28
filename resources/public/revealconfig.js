// Full list of configuration options available here:
// https://github.com/hakimel/reveal.js#configuration
Reveal.initialize({
controls: false,
progress: false,
history: false,
center: true,
slideNumber: 'c',
rollingLinks: false,
keyboard: true,
overview: true,
margin: 0.00,
minScale: 1.00,
maxScale: 1.40,
theme: Reveal.getQueryHash().theme, // available themes are in /css/theme
transition: Reveal.getQueryHash().transition || 'fade', // default/cube/page/concave/zoom/linear/fade/none
transitionSpeed: 'fast',
multiplex: {
    secret: '', // null if client
    id: '', // id, obtained from socket.io server
    url: '' // Location of socket.io server
}})
