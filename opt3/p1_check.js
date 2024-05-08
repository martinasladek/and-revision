<script>
var correctAnswer1 = 69.42; // Example numerical answer
var correctAnswer2 = "BDAC"; // Example text answer

function checkAnswers() {
    var answer1 = parseFloat(document.getElementById("answer1").value); // Convert input to number
    var answer2 = document.getElementById("answer2").value;
    
    // Check if answers are correct
    if (answer1 === correctAnswer1 && answer2 === "correctAnswer2") {
        // Link to another page
        window.location.href="https://sussex.ac.uk";
    } else {
        // Show error message
        alert('Incorrect answers. Please try again.');
    }
}
</script>