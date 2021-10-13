/**
 * Index.js handles the displaying, sizing, and updating of the 'site-map', the fixed
 * traffic-light structure that shows where you are on the page with lines (branches)
 * dynamically pointing to the .terminal-text headings as you navigate the page.
 */


console.log("foofoo");

////////////////////////////////////////////////////////////
// Get DOM elements
//
// Get terminal containers
const t1Cont  = document.querySelector('#terminal1');
const t2Cont  = document.querySelector('#terminal2');
const t3Cont  = document.querySelector('#terminal3');
const t4Cont  = document.querySelector('#terminal4');
const t5Cont  = document.querySelector('#terminal5');
// Branches
const branch1  = document.querySelector('#branch1');
const branch11 = document.querySelector('#branch11');
const branch12 = document.querySelector('#branch12');
//const branch2 = document.querySelector('#branch2');
const branch21 = document.querySelector('#branch21');
const branch22 = document.querySelector('#branch22');
//const branch3 = document.querySelector('#branch3');
const branch31 = document.querySelector('#branch31');
const branch32 = document.querySelector('#branch32');
//const branch2 = document.querySelector('#branch2');
const branch41 = document.querySelector('#branch41');
const branch42 = document.querySelector('#branch42');
//const branch2 = document.querySelector('#branch2');
const branch51 = document.querySelector('#branch51');
const branch52 = document.querySelector('#branch52');
// Circles
const circ1 = document.querySelector('#circle1');
const circ2 = document.querySelector('#circle2');
const circ3 = document.querySelector('#circle3');
const circ4 = document.querySelector('#circle4');


// Display all branches (hidden by defaut)
const branches = document.querySelectorAll('.branch');
branches.forEach(item => item.classList.remove('hidden'));


////////////////////////////////////////////////////////////
// Constants
//
// Pixels of vertical growth it takes for branch bend to reach rest
const n = 700;
// Margin between line/branch end and terminal text start
const marginPx = 5;
// Branch width
const bw = parseInt(window.getComputedStyle(branch1).getPropertyValue("border-top-width"));
// Circle border width (Need to ask a specific (-top-) width as firefox throughs a wobbly if not)
const cbw = parseInt(window.getComputedStyle(circ1).getPropertyValue("border-top-width"));

// Min proportion of gap between circle and heading for branch bend / vertical start
const min = 0.1;
// Max proportion of gap between circle and heading for branch bend / vertical start 
const max = 0.85;

////////////////////////////////////////////////////////////
// Direction variables, each branch, at the point where it bends (becomes vertical),
// will either be travelling up the page (if the .terminal-text it extends to is
// above) or down the page (if text is below).
var direction2  = 0;
var direction3  = 0;
var direction4  = 0;


/**
 * Set the current direction of a branch that is used when working out which circle
 * is the 'current' circle that should be filled in to tell the user where they are
 * on the page.
 *
 * @param branchNo  Num 1-4 but only branches 2-4 are expected to ever change direction.
 * @param direction 1 or 0, 1 for up, 0 for down.
 */
function setDirection(branchNo, direction) {
    switch (branchNo) {
    case 2:
	direction2 = direction;
	break;
    case 3:
	direction3 = direction;
	break;
    case 4:
	direction4 = direction;
	break;
    default:
	// Do Noting
	break;
    }
}

/**
 * Set the 'current' circle in the site map to indicate where you are on the page.
 * (If direction4 is 1 (up), then you must be at the bottom (circ4) of the page, 
 *  else if direction3 is 1 (up)... etc.)
 */
function setCurrentCircleInMap() {

    if (direction4) {
	circ1.classList.remove('current-circ');	
	circ2.classList.remove('current-circ');	
	circ3.classList.remove('current-circ');
	circ4.classList.add('current-circ');
	return;
    }
    if (direction3) {
	circ1.classList.remove('current-circ');	
	circ2.classList.remove('current-circ');	
	circ3.classList.add('current-circ');
	circ4.classList.remove('current-circ');
	return;
    }

    if (direction2) {
	circ1.classList.remove('current-circ');	
	circ2.classList.add('current-circ');	
	circ3.classList.remove('current-circ');
	circ4.classList.remove('current-circ');
	return;
    } else {
	circ1.classList.add('current-circ');	
	circ2.classList.remove('current-circ');	
	circ3.classList.remove('current-circ');
	circ4.classList.remove('current-circ');
    }
}

/**
 * Compute the point at which the branch should bend.
 * The bends in the branches flow around each other as the user scrolls the page. This
 * 'flow' happens smoothly according to a linear function based on h / (const n).
 * 
 * @param gap      the gap between the right edge of all the circles (the traffic-light)
 *                 elem, and the left edge of the leftmost '.terminal-text's/headings.
 * @param branchNo the current branchNo that is asking for its bend/vertical start.
 * @param isAbove  is the associated heading/terminal-text above or below current branch.
 * @param h        the height of the gap between the circle and the heading/terminal-text.
 */
function getVerticalStart(gap, branchNo, isAbove, h) {
    h = (h > n) ? n : h;
    k = h / n;

    // Arbitrary shrink ratio to reduce gap
    // between lines when lines heading up
    s = 0.8;
    
    if (isAbove) {
	
	initial = max;
	end = (min + ((branchNo - 1) * ((max - min) / 3))) * s;
	return  (initial + ((end - initial) * k)) * gap;
	
    } else {

	// Grow/shrink in other direction;
	k = 1 - k;
	
	initial = (min + ((4 - branchNo) * ((max - min) / 3)));
	end = max;
	return  (initial + ((end - initial) * k)) * gap;
    }
}

/**
 * Compute the offset between the top of the circle and the top or bottom of the branch
 *
 * cWidth (circleWidth) includes the circles border (cbw) but when using the offset via 
 * 'top: offset' or 'left: offset' it goes from inside the border so we need to - cbw
 */
function computeCircleOffset(cWidth) {
    // bw = branchWidth
    return ((cWidth - bw) / 2) - cbw;
}

/**
 * Compute the height that the branch should be to grow out of the CENTER of the circle 
 * and end pointing at CENTER of the text (assuming `top/bottom: circleOffset` is set)
 *
 * @param   circleOffset   the circleOffset computed by `computeCircleOffset()'
 *
 * @return  could be positive or negative (negative indicates that text is below circle)
 */
function computeHeight(cRect, tRect, circleOffset) {
    heightOffset = ((tRect.height - bw) / 2);

    difference = cRect.top - tRect.top; 
    
    if (difference > 0) { // .terminal-text is above circle
	return difference + circleOffset + bw + cbw - heightOffset;
    } else { // .terminal-text is below circle
	return difference + circleOffset - (bw / 2) - cbw - heightOffset;
    }
}

/**
 * Update the very top branch (the one that goes to the first heading: '~$whoami_')
 * This is the only branch that comes out the top of a circle and due to the headings
 * positino on the page, will never need to extend downwards. (text will always be above)
 *
 * @param    cRect      the rect object for the top circle
 */
function updateTopBranch(cRect) {

    t1Rect = t1Cont.getBoundingClientRect();
    cWidth = cRect.width;

    circleOffset = computeCircleOffset(cWidth);
    
    branchHeight = computeHeight(cRect, t1Rect, circleOffset) - circleOffset - bw;
    
    branch1.style.height = branchHeight + 'px';
    branch1.style.top = '-'+ branchHeight + 'px';
    
    branchWidth = (t1Rect.left - cRect.right) + cWidth/2;
    branch1.style.width = branchWidth - marginPx + 'px';
    branch1.style.left = circleOffset + 'px';
}

/**
 * Update one of the horizontal branches that extend out the right-hand side of a circle
 *
 * @param  cRect     the rect object of the circle this branch extends out of
 * @param  tRect     the rect object of the .terminal-text/heading this branch points to
 */
function updateHorizontalBranch(cRect , tRect, b1 , b2, gap, branchNo) {
        
    cWidth = cRect.width;

    circleOffset = computeCircleOffset(cWidth);

    // Return value could be positve or negative
    // (negative indicating heading/text is below circle)
    branchHeight = computeHeight(cRect, tRect, circleOffset);

    isAbove = (branchHeight > 0) ? 1 : 0;
    isBelow = (branchHeight > 0) ? 0 : 1;

    // Height need always be positive
    branchHeight *= (isBelow) ? -1 : 1;
    b1.style.height = branchHeight + 'px';
    b2.style.height = branchHeight + 'px';


    verticalOffset = circleOffset + 'px';
    
    if (isBelow) {
	b1.style.bottom = '';
	b2.style.bottom = '';
	b1.style.top = verticalOffset;
	b2.style.top = verticalOffset;

	b1.classList.remove('b-bottom');
	b1.classList.add('b-top');
	b2.classList.remove('b-top');
	b2.classList.add('b-bottom');

	setDirection(branchNo, isAbove);
	
    } else {
	b1.style.top = '';
	b2.style.top = '';
	b1.style.bottom = verticalOffset;
	b2.style.bottom = verticalOffset;
	
	b1.classList.remove('b-top');
	b1.classList.add('b-bottom');
	b2.classList.remove('b-bottom');
	b2.classList.add('b-top');

	setDirection(branchNo, isAbove);
    }

    
    branchWidth = (tRect.left - cRect.right);

    b1Width = getVerticalStart(gap, branchNo, isAbove, branchHeight);
    
    b2Width = (branchWidth - b1Width) - 2;

    
    b1.style.width = b1Width + 'px';
    b2.style.width = b2Width + 'px';

    
    b1.style.left = cRect.width - 2 + 'px';
    b2.style.left = (cRect.width + b1Width - 4) + 'px';
}

/**
 * Update all branches. Call everytime viewport is changed/move.
 */
function updateBranches() {
    c1Rect = circ1.getBoundingClientRect();
    updateTopBranch(c1Rect);

    t2Rect = t2Cont.getBoundingClientRect();
    t3Rect = t3Cont.getBoundingClientRect();
    t4Rect = t4Cont.getBoundingClientRect();
    t5Rect = t5Cont.getBoundingClientRect();

    // maxVerticalStar. Same for all cRects, so only compute once rather
    // than inside each call
    gap = t3Rect.left - c1Rect.right;
    
    updateHorizontalBranch(c1Rect, t2Rect, branch11, branch12, gap, 1);
    
    updateHorizontalBranch(circ2.getBoundingClientRect(), t3Rect, branch21, branch22, gap, 2);
    
    updateHorizontalBranch(circ3.getBoundingClientRect(), t4Rect, branch31, branch32, gap, 3);
    
    updateHorizontalBranch(circ4.getBoundingClientRect(), t5Rect, branch41, branch42, gap, 4);
    
    setCurrentCircleInMap();
    
}

/**
 * Update branches whenever the window is resized or scrolled.
 */
window.onload = function () {
    updateBranches();
    window.onscroll = updateBranches;
    window.onresize = updateBranches;
};

/**
 * Update branches every half second to account for show/hide menu shifting text up/down
 * This seems a little iffy but the updateBranches() function isn't too expensive to run
 * and we can't do an onClick as the nav menu eases in and out so can't pin down exactly
 * when the text/content will have moved.
 */
setInterval(function(){ 
    //code goes here that will be run every 0.25 seconds.
    updateBranches();
}, 250); 


