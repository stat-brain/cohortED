// Select the logo element
const logo = document.querySelector('.navbar-logo');

// Store the grayscale and color image paths
const greyLogo = './assets/img/logo_grey.png';
const colorLogo = './assets/img/logo_color.png';

// Change to color logo on hover
logo.addEventListener('mouseover', () => {
  logo.src = colorLogo;
});

// Revert to grayscale logo when the mouse leaves
logo.addEventListener('mouseout', () => {
  logo.src = greyLogo;
});
