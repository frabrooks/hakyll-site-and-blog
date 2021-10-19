
// Select DOM Items
const homeBtn = document.querySelector('#home-button');
const pageTitle = document.querySelector('#page-title');
const navLinks = document.querySelectorAll('.nav-link');

const showBtn = document.querySelector('#show-nav-links-button');

const nav1   = document.querySelector('#nav-link1');
const nav2   = document.querySelector('#nav-link2');
const nav3   = document.querySelector('#nav-link3');
const nav4   = document.querySelector('#nav-link4');
const nav5   = document.querySelector('#nav-link5');
const cvLink = document.querySelector('#cv-link');

let showMenu = false;


showBtn.addEventListener('click', toggleMenu);

function toggleMenu() {
    if (!showMenu) {
	
	try{
	    cvLink.classList.add('hide');
	} catch (_) {}
	
	homeBtn.classList.add('show');
	pageTitle.classList.add('show');
	showBtn.classList.add('show');
	showBtn.classList.add('close');
	navLinks.forEach(item => item.classList.add('show'));
	
    } else {

	try{
	    cvLink.classList.remove('hide');
	} catch (err) {}

	homeBtn.classList.remove('show');
	pageTitle.classList.remove('show');
	showBtn.classList.remove('show');
	showBtn.classList.remove('close');
	navLinks.forEach(item => item.classList.remove('show'));
	
    }
    showMenu = !showMenu;
}


// Disable nav menu item corresponding to the current page.
const cp = document.querySelector('meta[name="current-page"]').content;
switch (cp) {

case "Home":
    nav1.firstElementChild.href='';
    nav1.classList.add('current-page-link');
    nav1.classList.add('no-pointer-events');
    break;
case "Contact":
    nav5.firstElementChild.href='';
    nav5.classList.add('current-page-link');
    nav5.classList.add('no-pointer-events');
    break;
case "Blog":
    if(! nav2.classList.contains('isBlogPost')){
	nav2.classList.add('current-page-link');
	nav2.classList.add('no-pointer-events');
    }	
    break;
case "Open Source Journal":
    nav3.firstElementChild.hreh='';
    nav3.classList.add('current-page-link');
    nav3.classList.add('no-pointer-events');    
    break;    
default:
    break;
}


