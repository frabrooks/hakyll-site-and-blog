
// Select DOM Items

//const siteLogo = document.querySelector('#site-logo');
const pageTitle = document.querySelector('#page-title');
const navLinks = document.querySelectorAll('.nav-link');
const navEnd   = document.querySelector('.end-block');
const btnCont = document.querySelector('#button-container');
const menuBtn = document.querySelector('#menu-btn');
const nav1   = document.querySelector('#nav-link1');
const nav2   = document.querySelector('#nav-link2');
const nav3   = document.querySelector('#nav-link3');
const nav4   = document.querySelector('#nav-link4');
const nav5   = document.querySelector('#nav-link5');

let showMenu = false;

btnCont.addEventListener('click', toggleMenu);

function toggleMenu() {
    if (!showMenu) {
	pageTitle.classList.add('show');
	//siteLogo.classList.add('show');
	navEnd.classList.add('show');
	nav1.classList.add('title-hidden');
	menuBtn.classList.add('close');
	navLinks.forEach(item => item.classList.add('show'));
    } else {
	pageTitle.classList.remove('show');
	//siteLogo.classList.remove('show');
	navEnd.classList.remove('show');
	nav1.classList.remove('title-hidden');
	menuBtn.classList.remove('close');
	navLinks.forEach(item => item.classList.remove('show'));
    }
    showMenu = !showMenu;
}


const cp = document.querySelector('meta[name="current-page"]').content;


switch (cp) {

case "Home":
    nav1.classList.add('current');
    nav1.classList.remove('clickable');
    break;
case "Contact":
    nav5.classList.add('current');
    nav5.classList.remove('clickable');
    break;
default:
    // Do something stupid so I don't forget to
    // update this js when changing names :)
    menuBtn.style.backgroundColor = "red";
    pageTitle.style.background = "url(../images/oops.jpg)";
    pageTitle.style.backgroundSize = "100% 100%";
    break;
}


