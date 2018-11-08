function toggleMenu(){
    var element = document.querySelector(".menu");
    element.classList.toggle("show");

    element = document.querySelector(".menuButton");
    element.classList.toggle("active");

    element = document.querySelector(".overlay");
    element.classList.toggle("displayed");
}