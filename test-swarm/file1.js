// Sample JavaScript file
function calculateTotal(items) {
  return items.reduce((sum, item) => sum + item.price, 0);
}

module.exports = { calculateTotal };