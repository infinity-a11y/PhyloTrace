document.getElementById('newsletter-form').addEventListener('submit', function(event) {
    event.preventDefault();
    var email = document.getElementById('email').value;
    // Here you would send the email to your server or a service like Mailchimp
    alert('Thank you for subscribing!');
});
