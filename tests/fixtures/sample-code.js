// Sample JavaScript code for testing
function calculateSum(numbers) {
  return numbers.reduce((sum, num) => sum + num, 0);
}

function findMax(numbers) {
  return Math.max(...numbers);
}

module.exports = {
  calculateSum,
  findMax
};