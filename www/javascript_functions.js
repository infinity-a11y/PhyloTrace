// Copy sequence clipboard
Shiny.addCustomMessageHandler('txt', function (txt) {
  navigator.clipboard.writeText(txt);
});

// Get date format
function getCurrentDate() {
  var currentDate = new Date();
  var year = currentDate.getFullYear();
  
  // Get month and day with padding since JavaScript months are 0-based
  var month = String(currentDate.getMonth() + 1).padStart(2, '0'); // Adds leading 0 if needed
  var day = String(currentDate.getDate()).padStart(2, '0'); // Adds leading 0 if needed
  
  return year + '-' + month + '-' + day; // Formats the date as "YYYY-MM-DD"
};

function checkAndExecute() {
  if(document.querySelector("#loaded_scheme > div > li > span") !== null) {
    // Select the span element
    let spanElement = document.querySelector("#loaded_scheme > div > li > span");
              
    // Get the bounding rectangle of the span element
    let rect = spanElement.getBoundingClientRect();
    
    // Extract the width
    let width = rect.width;
    
    Shiny.setInputValue("scheme_position", width);            
  }
}
setInterval(checkAndExecute, 1000);
checkAndExecute();

// Get time
function updateTime() {
  var options = {
    timeZone: Intl.DateTimeFormat().resolvedOptions().timeZone,
    hour: '2-digit',
    minute: '2-digit',
    second: '2-digit',
    hour12: false,
    timeZoneName: 'short'
  };
  var currentTime = new Date().toLocaleTimeString([], options);
  document.getElementById('currentTime').innerHTML = currentTime;
  
}
setInterval(updateTime, 1000);
updateTime();

//MST png to report
function mstReport() {
  var canvases = document.querySelectorAll('canvas');
  
  if(canvases.length < 2) {
    // Get all canvas elements on the document
    var canvas = document.querySelector('canvas');
    
    // Assuming the canvases are ordered as title, subtitle, main, and footer
    var titleCanvasID = document.getElementById('titletree_mst');
    if(!(titleCanvasID.innerText.length === 0)) {
      var titleCanvas = document.createElement('canvas');
      var titleCanvasHeight = titleCanvasID.offsetHeight;
      titleCanvas.width = canvas.width;
      titleCanvas.height = titleCanvasHeight;
      
      // Draw title text on titleCanvas
      var titleCtx = titleCanvas.getContext('2d');
      titleCtx.font = window.getComputedStyle(titleCanvasID).getPropertyValue('font');
      titleCtx.textAlign = window.getComputedStyle(titleCanvasID).getPropertyValue('text-align');
      titleCtx.fillStyle = window.getComputedStyle(titleCanvasID).getPropertyValue('color');
      titleCtx.fillText(titleCanvasID.innerText, titleCanvas.width / 2, titleCanvasHeight); 
    } else {var titleCanvasHeight = 0;}
    var subtitleCanvasID = document.getElementById('subtitletree_mst');
    if(!(subtitleCanvasID.innerText.length === 0)) {
      var subtitleCanvas = document.createElement('canvas');
      var subtitleCanvasHeight = subtitleCanvasID.offsetHeight;
      subtitleCanvas.width = canvas.width;
      subtitleCanvas.height = subtitleCanvasHeight;
      
      // Draw subtitle text on subtitleCanvas
      var subtitleCtx = subtitleCanvas.getContext('2d');
      subtitleCtx.font = window.getComputedStyle(subtitleCanvasID).getPropertyValue('font');
      subtitleCtx.textAlign = window.getComputedStyle(subtitleCanvasID).getPropertyValue('text-align');
      subtitleCtx.fillStyle = window.getComputedStyle(subtitleCanvasID).getPropertyValue('color');
      subtitleCtx.fillText(subtitleCanvasID.innerText, subtitleCanvas.width / 2, subtitleCanvasHeight);
    } else {var subtitleCanvasHeight = 0;}
    
    // Get the heights of the canvas
    var mainCanvasHeight = canvas.height;
    
    // Get the main canvas context
    var mainCtx = canvas.getContext('2d');
    var mainCanvasWidth = canvas.width;
    
    // Create a new canvas to merge title, subtitle, main plot, and footer
    var mergedCanvas = document.createElement('canvas');
    mergedCanvas.width = canvas.width;
    mergedCanvas.height = mainCanvasHeight + titleCanvasHeight + subtitleCanvasHeight;
    
    var ctx = mergedCanvas.getContext('2d');
    
    // Draw title, subtitle, main plot, and footer onto the merged canvas
    ctx.fillStyle = getBackgroundColor();
    ctx.fillRect(0, 0, canvas.width, mergedCanvas.height);
    if(!(titleCanvasID.innerText.length === 0)) {
      ctx.drawImage(titleCanvas, 0, 0);
    }
    if(!(subtitleCanvasID.innerText.length === 0)) {
      ctx.drawImage(subtitleCanvas, 0, titleCanvasHeight);
    }
    ctx.drawImage(canvas, 0, titleCanvasHeight + subtitleCanvasHeight);
  } else {
    var legendCanvas = canvases[0];
    var plotCanvas = canvases[1];
    
    // Assuming the canvases are ordered as title, subtitle, main, and footer
    var titleCanvasID = document.getElementById('titletree_mst');
    if(!(titleCanvasID.innerText.length === 0)) {
      var titleCanvas = document.createElement('canvas');
      var titleCanvasHeight = titleCanvasID.offsetHeight;
      titleCanvas.width = legendCanvas.width + plotCanvas.width;
      titleCanvas.height = titleCanvasHeight;
      
      // Draw title text on titleCanvas
      var titleCtx = titleCanvas.getContext('2d');
      titleCtx.font = window.getComputedStyle(titleCanvasID).getPropertyValue('font');
      titleCtx.textAlign = window.getComputedStyle(titleCanvasID).getPropertyValue('left');
      titleCtx.fillStyle = window.getComputedStyle(titleCanvasID).getPropertyValue('color');
      titleCtx.fillText(titleCanvasID.innerText, titleCanvas.width / 20, titleCanvasHeight / 1.5); 
    } else {var titleCanvasHeight = 0;}
    
    var subtitleCanvasID = document.getElementById('subtitletree_mst');
    if(!(subtitleCanvasID.innerText.length === 0)) {
      var subtitleCanvas = document.createElement('canvas');
      var subtitleCanvasHeight = subtitleCanvasID.offsetHeight;
      subtitleCanvas.width = legendCanvas.width + plotCanvas.width;
      subtitleCanvas.height = subtitleCanvasHeight;
      
      // Draw subtitle text on subtitleCanvas
      var subtitleCtx = subtitleCanvas.getContext('2d');
      subtitleCtx.font = window.getComputedStyle(subtitleCanvasID).getPropertyValue('font');
      subtitleCtx.textAlign = window.getComputedStyle(subtitleCanvasID).getPropertyValue('text-align');
      subtitleCtx.fillStyle = window.getComputedStyle(subtitleCanvasID).getPropertyValue('color');
      subtitleCtx.fillText(subtitleCanvasID.innerText, subtitleCanvas.width / 20, subtitleCanvasHeight / 1.5);
    } else {var subtitleCanvasHeight = 0;}  
    
    // Get the heights of the canvas
    var mainCanvasHeight = plotCanvas.height;
    
    // Create a new canvas to merge title, subtitle, main plot, and footer
    var mergedCanvas = document.createElement('canvas');
    mergedCanvas.width = legendCanvas.width + plotCanvas.width;
    mergedCanvas.height = mainCanvasHeight + titleCanvasHeight + subtitleCanvasHeight;
    var ctx = mergedCanvas.getContext('2d');
    
    // Draw background
    ctx.fillStyle = getBackgroundColorClear();
    ctx.fillRect(0, 0, mergedCanvas.width, mergedCanvas.height);
    
    // Draw title
    if (titleCanvasID && titleCanvasID.innerText.length !== 0) {
      ctx.drawImage(titleCanvas, 0, 0);
    }
    
    // Draw subtitle
    if (subtitleCanvasID && subtitleCanvasID.innerText.length !== 0) {
      ctx.drawImage(subtitleCanvas, 0, titleCanvasHeight);
    }
    
    // Draw legend and plot canvases side by side
    ctx.drawImage(legendCanvas, 0, titleCanvasHeight + subtitleCanvasHeight);
    ctx.drawImage(plotCanvas, legendCanvas.width, titleCanvasHeight + subtitleCanvasHeight);
  }
  
  // Convert the canvas to a data URL
  var dataURL = mergedCanvas.toDataURL('image/jpeg', 1.0);
  
  // Extract the base64-encoded string
  var base64Data = dataURL.split(',')[1];
  
  console.log(base64Data);
  
  // Send the base64-encoded string to the server
  Shiny.setInputValue('canvas_data', base64Data);
  
  mergedCanvas.remove();  
}

// MST jpeg download
$(document).on('click', '#save_plot_jpeg', function() {
  
  var canvases = document.querySelectorAll('canvas');
  
  if(canvases.length < 2) {
    // Get all canvas elements on the document
    var canvas = document.querySelector('canvas');
    
    // Assuming the canvases are ordered as title, subtitle, main, and footer
    var titleCanvasID = document.getElementById('titletree_mst');
    if(!(titleCanvasID.innerText.length === 0)) {
      var titleCanvas = document.createElement('canvas');
      var titleCanvasHeight = titleCanvasID.offsetHeight;
      titleCanvas.width = canvas.width;
      titleCanvas.height = titleCanvasHeight;
      
      // Draw title text on titleCanvas
      var titleCtx = titleCanvas.getContext('2d');
      titleCtx.font = window.getComputedStyle(titleCanvasID).getPropertyValue('font');
      titleCtx.textAlign = window.getComputedStyle(titleCanvasID).getPropertyValue('text-align');
      titleCtx.fillStyle = window.getComputedStyle(titleCanvasID).getPropertyValue('color');
      titleCtx.fillText(titleCanvasID.innerText, titleCanvas.width / 2, titleCanvasHeight); 
    } else {var titleCanvasHeight = 0;}
    var subtitleCanvasID = document.getElementById('subtitletree_mst');
    if(!(subtitleCanvasID.innerText.length === 0)) {
      var subtitleCanvas = document.createElement('canvas');
      var subtitleCanvasHeight = subtitleCanvasID.offsetHeight;
      subtitleCanvas.width = canvas.width;
      subtitleCanvas.height = subtitleCanvasHeight;
      
      // Draw subtitle text on subtitleCanvas
      var subtitleCtx = subtitleCanvas.getContext('2d');
      subtitleCtx.font = window.getComputedStyle(subtitleCanvasID).getPropertyValue('font');
      subtitleCtx.textAlign = window.getComputedStyle(subtitleCanvasID).getPropertyValue('text-align');
      subtitleCtx.fillStyle = window.getComputedStyle(subtitleCanvasID).getPropertyValue('color');
      subtitleCtx.fillText(subtitleCanvasID.innerText, subtitleCanvas.width / 2, subtitleCanvasHeight);
    } else {var subtitleCanvasHeight = 0;}
    
    // Get the heights of the canvas
    var mainCanvasHeight = canvas.height;
    
    // Get the main canvas context
    var mainCtx = canvas.getContext('2d');
    var mainCanvasWidth = canvas.width;
    
    // Create a new canvas to merge title, subtitle, main plot, and footer
    var mergedCanvas = document.createElement('canvas');
    mergedCanvas.width = canvas.width;
    mergedCanvas.height = mainCanvasHeight + titleCanvasHeight + subtitleCanvasHeight;
    
    var ctx = mergedCanvas.getContext('2d');
    
    // Draw title, subtitle, main plot, and footer onto the merged canvas
    ctx.fillStyle = getBackgroundColor();
    ctx.fillRect(0, 0, canvas.width, mergedCanvas.height);
    if(!(titleCanvasID.innerText.length === 0)) {
      ctx.drawImage(titleCanvas, 0, 0);
    }
    if(!(subtitleCanvasID.innerText.length === 0)) {
      ctx.drawImage(subtitleCanvas, 0, titleCanvasHeight);
    }
    ctx.drawImage(canvas, 0, titleCanvasHeight + subtitleCanvasHeight);
  } else {
    var legendCanvas = canvases[0];
    var plotCanvas = canvases[1];
    
    // Assuming the canvases are ordered as title, subtitle, main, and footer
    var titleCanvasID = document.getElementById('titletree_mst');
    if(!(titleCanvasID.innerText.length === 0)) {
      var titleCanvas = document.createElement('canvas');
      var titleCanvasHeight = titleCanvasID.offsetHeight;
      titleCanvas.width = legendCanvas.width + plotCanvas.width;
      titleCanvas.height = titleCanvasHeight;
      
      // Draw title text on titleCanvas
      var titleCtx = titleCanvas.getContext('2d');
      titleCtx.font = window.getComputedStyle(titleCanvasID).getPropertyValue('font');
      titleCtx.textAlign = window.getComputedStyle(titleCanvasID).getPropertyValue('left');
      titleCtx.fillStyle = window.getComputedStyle(titleCanvasID).getPropertyValue('color');
      titleCtx.fillText(titleCanvasID.innerText, titleCanvas.width / 20, titleCanvasHeight / 1.5); 
    } else {var titleCanvasHeight = 0;}
    
    var subtitleCanvasID = document.getElementById('subtitletree_mst');
    if(!(subtitleCanvasID.innerText.length === 0)) {
      var subtitleCanvas = document.createElement('canvas');
      var subtitleCanvasHeight = subtitleCanvasID.offsetHeight;
      subtitleCanvas.width = legendCanvas.width + plotCanvas.width;
      subtitleCanvas.height = subtitleCanvasHeight;
      
      // Draw subtitle text on subtitleCanvas
      var subtitleCtx = subtitleCanvas.getContext('2d');
      subtitleCtx.font = window.getComputedStyle(subtitleCanvasID).getPropertyValue('font');
      subtitleCtx.textAlign = window.getComputedStyle(subtitleCanvasID).getPropertyValue('text-align');
      subtitleCtx.fillStyle = window.getComputedStyle(subtitleCanvasID).getPropertyValue('color');
      subtitleCtx.fillText(subtitleCanvasID.innerText, subtitleCanvas.width / 20, subtitleCanvasHeight / 1.5);
    } else {var subtitleCanvasHeight = 0;}  
    
    // Get the heights of the canvas
    var mainCanvasHeight = plotCanvas.height;
    
    // Create a new canvas to merge title, subtitle, main plot, and footer
    var mergedCanvas = document.createElement('canvas');
    mergedCanvas.width = legendCanvas.width + plotCanvas.width;
    mergedCanvas.height = mainCanvasHeight + titleCanvasHeight + subtitleCanvasHeight;
    var ctx = mergedCanvas.getContext('2d');
    
    // Draw background
    ctx.fillStyle = getBackgroundColorClear();
    ctx.fillRect(0, 0, mergedCanvas.width, mergedCanvas.height);
    
    // Draw title
    if (titleCanvasID && titleCanvasID.innerText.length !== 0) {
      ctx.drawImage(titleCanvas, 0, 0);
    }
    
    // Draw subtitle
    if (subtitleCanvasID && subtitleCanvasID.innerText.length !== 0) {
      ctx.drawImage(subtitleCanvas, 0, titleCanvasHeight);
    }
    
    // Draw legend and plot canvases side by side
    ctx.drawImage(legendCanvas, 0, titleCanvasHeight + subtitleCanvasHeight);
    ctx.drawImage(plotCanvas, legendCanvas.width, titleCanvasHeight + subtitleCanvasHeight);
  }
  
  // Download the merged canvas as a JPEG image
  const a = document.createElement('a');
  document.body.append(a);
  a.download = getCurrentDate()+'_MST';
  a.href = mergedCanvas.toDataURL('image/jpeg', 1.0);
  a.click();
  a.remove();
  
  // Remove the dynamically created merged canvas
  mergedCanvas.remove();
  
});

// MST png Download
$(document).on('click', '#save_plot_png', function() {
  
  var canvases = document.querySelectorAll('canvas');
  
  if(canvases.length < 2) {
    // Get all canvas elements on the document
    var canvas = document.querySelector('canvas');
    
    // Assuming the canvases are ordered as title, subtitle, main, and footer
    var titleCanvasID = document.getElementById('titletree_mst');
    if(!(titleCanvasID.innerText.length === 0)) {
      var titleCanvas = document.createElement('canvas');
      var titleCanvasHeight = titleCanvasID.offsetHeight;
      titleCanvas.width = canvas.width;
      titleCanvas.height = titleCanvasHeight;
      
      // Draw title text on titleCanvas
      var titleCtx = titleCanvas.getContext('2d');
      titleCtx.font = window.getComputedStyle(titleCanvasID).getPropertyValue('font');
      titleCtx.textAlign = window.getComputedStyle(titleCanvasID).getPropertyValue('text-align');
      titleCtx.fillStyle = window.getComputedStyle(titleCanvasID).getPropertyValue('color');
      titleCtx.fillText(titleCanvasID.innerText, titleCanvas.width / 2, titleCanvasHeight); 
    } else {var titleCanvasHeight = 0;}
    var subtitleCanvasID = document.getElementById('subtitletree_mst');
    if(!(subtitleCanvasID.innerText.length === 0)) {
      var subtitleCanvas = document.createElement('canvas');
      var subtitleCanvasHeight = subtitleCanvasID.offsetHeight;
      subtitleCanvas.width = canvas.width;
      subtitleCanvas.height = subtitleCanvasHeight;
      
      // Draw subtitle text on subtitleCanvas
      var subtitleCtx = subtitleCanvas.getContext('2d');
      subtitleCtx.font = window.getComputedStyle(subtitleCanvasID).getPropertyValue('font');
      subtitleCtx.textAlign = window.getComputedStyle(subtitleCanvasID).getPropertyValue('text-align');
      subtitleCtx.fillStyle = window.getComputedStyle(subtitleCanvasID).getPropertyValue('color');
      subtitleCtx.fillText(subtitleCanvasID.innerText, subtitleCanvas.width / 2, subtitleCanvasHeight);
    } else {var subtitleCanvasHeight = 0;}
    
    // Get the heights of the canvas
    var mainCanvasHeight = canvas.height;
    
    // Get the main canvas context
    var mainCtx = canvas.getContext('2d');
    var mainCanvasWidth = canvas.width;
    
    // Create a new canvas to merge title, subtitle, main plot, and footer
    var mergedCanvas = document.createElement('canvas');
    mergedCanvas.width = canvas.width;
    mergedCanvas.height = mainCanvasHeight + titleCanvasHeight + subtitleCanvasHeight;
    
    var ctx = mergedCanvas.getContext('2d');
    
    // Draw title, subtitle, main plot, and footer onto the merged canvas
    ctx.fillStyle = getBackgroundColor();
    ctx.fillRect(0, 0, canvas.width, mergedCanvas.height);
    if(!(titleCanvasID.innerText.length === 0)) {
      ctx.drawImage(titleCanvas, 0, 0);
    }
    if(!(subtitleCanvasID.innerText.length === 0)) {
      ctx.drawImage(subtitleCanvas, 0, titleCanvasHeight);
    }
    ctx.drawImage(canvas, 0, titleCanvasHeight + subtitleCanvasHeight);
  } else {
    var legendCanvas = canvases[0];
    var plotCanvas = canvases[1];
    
    // Assuming the canvases are ordered as title, subtitle, main, and footer
    var titleCanvasID = document.getElementById('titletree_mst');
    if(!(titleCanvasID.innerText.length === 0)) {
      var titleCanvas = document.createElement('canvas');
      var titleCanvasHeight = titleCanvasID.offsetHeight;
      titleCanvas.width = legendCanvas.width + plotCanvas.width;
      titleCanvas.height = titleCanvasHeight;
      
      // Draw title text on titleCanvas
      var titleCtx = titleCanvas.getContext('2d');
      titleCtx.font = window.getComputedStyle(titleCanvasID).getPropertyValue('font');
      titleCtx.textAlign = window.getComputedStyle(titleCanvasID).getPropertyValue('left');
      titleCtx.fillStyle = window.getComputedStyle(titleCanvasID).getPropertyValue('color');
      titleCtx.fillText(titleCanvasID.innerText, titleCanvas.width / 20, titleCanvasHeight / 1.5); 
    } else {var titleCanvasHeight = 0;}
    
    var subtitleCanvasID = document.getElementById('subtitletree_mst');
    if(!(subtitleCanvasID.innerText.length === 0)) {
      var subtitleCanvas = document.createElement('canvas');
      var subtitleCanvasHeight = subtitleCanvasID.offsetHeight;
      subtitleCanvas.width = legendCanvas.width + plotCanvas.width;
      subtitleCanvas.height = subtitleCanvasHeight;
      
      // Draw subtitle text on subtitleCanvas
      var subtitleCtx = subtitleCanvas.getContext('2d');
      subtitleCtx.font = window.getComputedStyle(subtitleCanvasID).getPropertyValue('font');
      subtitleCtx.textAlign = window.getComputedStyle(subtitleCanvasID).getPropertyValue('text-align');
      subtitleCtx.fillStyle = window.getComputedStyle(subtitleCanvasID).getPropertyValue('color');
      subtitleCtx.fillText(subtitleCanvasID.innerText, subtitleCanvas.width / 20, subtitleCanvasHeight / 1.5);
    } else {var subtitleCanvasHeight = 0;}  
    
    // Get the heights of the canvas
    var mainCanvasHeight = plotCanvas.height;
    
    // Create a new canvas to merge title, subtitle, main plot, and footer
    var mergedCanvas = document.createElement('canvas');
    mergedCanvas.width = legendCanvas.width + plotCanvas.width;
    mergedCanvas.height = mainCanvasHeight + titleCanvasHeight + subtitleCanvasHeight;
    var ctx = mergedCanvas.getContext('2d');
    
    // Draw background
    ctx.fillStyle = getBackgroundColorClear();
    ctx.fillRect(0, 0, mergedCanvas.width, mergedCanvas.height);
    
    // Draw title
    if (titleCanvasID && titleCanvasID.innerText.length !== 0) {
      ctx.drawImage(titleCanvas, 0, 0);
    }
    
    // Draw subtitle
    if (subtitleCanvasID && subtitleCanvasID.innerText.length !== 0) {
      ctx.drawImage(subtitleCanvas, 0, titleCanvasHeight);
    }
    
    // Draw legend and plot canvases side by side
    ctx.drawImage(legendCanvas, 0, titleCanvasHeight + subtitleCanvasHeight);
    ctx.drawImage(plotCanvas, legendCanvas.width, titleCanvasHeight + subtitleCanvasHeight);
  }
  
  // Download the merged canvas as a PNG image
  const a = document.createElement('a');
  document.body.append(a);
  a.download = getCurrentDate()+'_MST.png';
  a.href = mergedCanvas.toDataURL('image/png');
  a.click();
  a.remove();
  
  // Remove the dynamically created merged canvas
  mergedCanvas.remove();    
  
});

// MST bmp Download
$(document).on('click', '#save_plot_bmp', function() {
  
  var canvases = document.querySelectorAll('canvas');
  
  if(canvases.length < 2) {
    // Get all canvas elements on the document
    var canvas = document.querySelector('canvas');
    
    // Assuming the canvases are ordered as title, subtitle, main, and footer
    var titleCanvasID = document.getElementById('titletree_mst');
    if(!(titleCanvasID.innerText.length === 0)) {
      var titleCanvas = document.createElement('canvas');
      var titleCanvasHeight = titleCanvasID.offsetHeight;
      titleCanvas.width = canvas.width;
      titleCanvas.height = titleCanvasHeight;
      
      // Draw title text on titleCanvas
      var titleCtx = titleCanvas.getContext('2d');
      titleCtx.font = window.getComputedStyle(titleCanvasID).getPropertyValue('font');
      titleCtx.textAlign = window.getComputedStyle(titleCanvasID).getPropertyValue('text-align');
      titleCtx.fillStyle = window.getComputedStyle(titleCanvasID).getPropertyValue('color');
      titleCtx.fillText(titleCanvasID.innerText, titleCanvas.width / 2, titleCanvasHeight); 
    } else {var titleCanvasHeight = 0;}
    var subtitleCanvasID = document.getElementById('subtitletree_mst');
    if(!(subtitleCanvasID.innerText.length === 0)) {
      var subtitleCanvas = document.createElement('canvas');
      var subtitleCanvasHeight = subtitleCanvasID.offsetHeight;
      subtitleCanvas.width = canvas.width;
      subtitleCanvas.height = subtitleCanvasHeight;
      
      // Draw subtitle text on subtitleCanvas
      var subtitleCtx = subtitleCanvas.getContext('2d');
      subtitleCtx.font = window.getComputedStyle(subtitleCanvasID).getPropertyValue('font');
      subtitleCtx.textAlign = window.getComputedStyle(subtitleCanvasID).getPropertyValue('text-align');
      subtitleCtx.fillStyle = window.getComputedStyle(subtitleCanvasID).getPropertyValue('color');
      subtitleCtx.fillText(subtitleCanvasID.innerText, subtitleCanvas.width / 2, subtitleCanvasHeight);
    } else {var subtitleCanvasHeight = 0;}
    
    // Get the heights of the canvas
    var mainCanvasHeight = canvas.height;
    
    // Get the main canvas context
    var mainCtx = canvas.getContext('2d');
    var mainCanvasWidth = canvas.width;
    
    // Create a new canvas to merge title, subtitle, main plot, and footer
    var mergedCanvas = document.createElement('canvas');
    mergedCanvas.width = canvas.width;
    mergedCanvas.height = mainCanvasHeight + titleCanvasHeight + subtitleCanvasHeight;
    
    var ctx = mergedCanvas.getContext('2d');
    
    // Draw title, subtitle, main plot, and footer onto the merged canvas
    ctx.fillStyle = getBackgroundColor();
    ctx.fillRect(0, 0, canvas.width, mergedCanvas.height);
    if(!(titleCanvasID.innerText.length === 0)) {
      ctx.drawImage(titleCanvas, 0, 0);
    }
    if(!(subtitleCanvasID.innerText.length === 0)) {
      ctx.drawImage(subtitleCanvas, 0, titleCanvasHeight);
    }
    ctx.drawImage(canvas, 0, titleCanvasHeight + subtitleCanvasHeight);
  } else {
    var legendCanvas = canvases[0];
    var plotCanvas = canvases[1];
    
    // Assuming the canvases are ordered as title, subtitle, main, and footer
    var titleCanvasID = document.getElementById('titletree_mst');
    if(!(titleCanvasID.innerText.length === 0)) {
      var titleCanvas = document.createElement('canvas');
      var titleCanvasHeight = titleCanvasID.offsetHeight;
      titleCanvas.width = legendCanvas.width + plotCanvas.width;
      titleCanvas.height = titleCanvasHeight;
      
      // Draw title text on titleCanvas
      var titleCtx = titleCanvas.getContext('2d');
      titleCtx.font = window.getComputedStyle(titleCanvasID).getPropertyValue('font');
      titleCtx.textAlign = window.getComputedStyle(titleCanvasID).getPropertyValue('left');
      titleCtx.fillStyle = window.getComputedStyle(titleCanvasID).getPropertyValue('color');
      titleCtx.fillText(titleCanvasID.innerText, titleCanvas.width / 20, titleCanvasHeight / 1.5); 
    } else {var titleCanvasHeight = 0;}
    
    var subtitleCanvasID = document.getElementById('subtitletree_mst');
    if(!(subtitleCanvasID.innerText.length === 0)) {
      var subtitleCanvas = document.createElement('canvas');
      var subtitleCanvasHeight = subtitleCanvasID.offsetHeight;
      subtitleCanvas.width = legendCanvas.width + plotCanvas.width;
      subtitleCanvas.height = subtitleCanvasHeight;
      
      // Draw subtitle text on subtitleCanvas
      var subtitleCtx = subtitleCanvas.getContext('2d');
      subtitleCtx.font = window.getComputedStyle(subtitleCanvasID).getPropertyValue('font');
      subtitleCtx.textAlign = window.getComputedStyle(subtitleCanvasID).getPropertyValue('text-align');
      subtitleCtx.fillStyle = window.getComputedStyle(subtitleCanvasID).getPropertyValue('color');
      subtitleCtx.fillText(subtitleCanvasID.innerText, subtitleCanvas.width / 20, subtitleCanvasHeight / 1.5);
    } else {var subtitleCanvasHeight = 0;}  
    
    // Get the heights of the canvas
    var mainCanvasHeight = plotCanvas.height;
    
    // Create a new canvas to merge title, subtitle, main plot, and footer
    var mergedCanvas = document.createElement('canvas');
    mergedCanvas.width = legendCanvas.width + plotCanvas.width;
    mergedCanvas.height = mainCanvasHeight + titleCanvasHeight + subtitleCanvasHeight;
    var ctx = mergedCanvas.getContext('2d');
    
    // Draw background
    ctx.fillStyle = getBackgroundColorClear();
    ctx.fillRect(0, 0, mergedCanvas.width, mergedCanvas.height);
    
    // Draw title
    if (titleCanvasID && titleCanvasID.innerText.length !== 0) {
      ctx.drawImage(titleCanvas, 0, 0);
    }
    
    // Draw subtitle
    if (subtitleCanvasID && subtitleCanvasID.innerText.length !== 0) {
      ctx.drawImage(subtitleCanvas, 0, titleCanvasHeight);
    }
    
    // Draw legend and plot canvases side by side
    ctx.drawImage(legendCanvas, 0, titleCanvasHeight + subtitleCanvasHeight);
    ctx.drawImage(plotCanvas, legendCanvas.width, titleCanvasHeight + subtitleCanvasHeight);
  }
  
  // Download the merged canvas as a bmp image
  const a = document.createElement('a');
  document.body.append(a);
  a.download = getCurrentDate()+'_MST.bmp';
  a.href = mergedCanvas.toDataURL('image/bmp');
  a.click();
  a.remove();
  // Remove the dynamically created merged canvas
  mergedCanvas.remove();
  
});

// Function to get the RGB color from a specific sub-element with a specific ID and class
function getBackgroundColorClear() {
  // Check the state of the Shiny checkbox 
  var checkboxStatus = $('#mst_background_transparent').prop('checked');
  
  // If the checkbox is checked, return transparent RGB code
  if (checkboxStatus) {
    return 'rgba(0, 0, 0, 0)';
  } else {
    // Get the sub-element by ID and class
    var targetElement = document.querySelector('#mst_background_color input.form-control.pickr-color');
    
    // Check if the element is found
    if (targetElement) {
      // Get the computed style of the element
      var computedStyle = window.getComputedStyle(targetElement);
      
      // Get the background color property
      var backgroundColor = computedStyle.backgroundColor;
      
      // Parse the RGB values from the string
      var rgbArray = backgroundColor.match(/\d+/g);
      
      // Convert the RGB values to a formatted string
      var rgbString = 'rgb(' + rgbArray.join(', ') + ')';
      
      // Return the RGB string
      return rgbString;
    } else {
      console.error('Element not found.');
      return null; // or any default value you want to return
    }
  }
};

// Function to get the RGB color from a specific sub-element with a specific ID and class
function getBackgroundColor() {
  // Check the state of the Shiny checkbox 
  var checkboxStatus = $('#mst_background_transparent').prop('checked');
  
  // If the checkbox is checked, return transparent RGB code
  if (checkboxStatus) {
    return 'rgb(255, 255, 255)';
  } else {
    // Get the sub-element by ID and class
    var targetElement = document.querySelector('#mst_background_color input.form-control.pickr-color');
    // Check if the element is found
    if (targetElement) {
      // Get the computed style of the element
      var computedStyle = window.getComputedStyle(targetElement);
      // Get the background color property
      var backgroundColor = computedStyle.backgroundColor;
      // Parse the RGB values from the string
      var rgbArray = backgroundColor.match(/\d+/g);
      // Convert the RGB values to a formatted string
      var rgbString = 'rgb(' + rgbArray.join(', ') + ')';
      // Return the RGB string
      return rgbString;
    } else {
      console.error('Element not found.');
      return null; // or any default value you want to return
    }
  }
};

// Restrict special characters using JA

// Function to apply JavaScript code
function applyJavaScript_assembly_id() {
  $("#assembly_id").on("input", function() {
    var value = $(this).val();
    $(this).val(value.replace(/[^a-zA-Z0-9_\\. -]/g, ""));
  });
}

// Check if the textInput element is available
function checkElement_assembly_id() {
  if ($("#assembly_id").length) {
    applyJavaScript_assembly_id();
    setTimeout(checkElement_assembly_id, 100);
  } else {
    setTimeout(checkElement_assembly_id, 100); // Check again in 100 milliseconds
  }
}

// Initial check on document ready
$(document).ready(function() {
  checkElement_assembly_id();
});

// Function to apply JavaScript code
function applyJavaScript_assembly_name() {
  $("#assembly_name").on("input", function() {
    var value = $(this).val();
    $(this).val(value.replace(/[^a-zA-Z0-9_\\. -]/g, ""));
  });
}

// Check if the textInput element is available
function checkElement_assembly_name() {
  if ($("#assembly_name").length) {
    applyJavaScript_assembly_name();
    setTimeout(checkElement_assembly_name, 100);
  } else {
    setTimeout(checkElement_assembly_name, 100); // Check again in 100 milliseconds
  }
}

// Initial check on document ready
$(document).ready(function() {
  checkElement_assembly_name();
});

// Function to apply JavaScript code
function applyJavaScript_append_host() {
  $("#append_host").on("input", function() {
    var value = $(this).val();
    $(this).val(value.replace(/[^a-zA-Z0-9_\\. -]/g, ""));
  });
}

// Check if the textInput element is available
function checkElement_append_host() {
  if ($("#append_host").length) {
    applyJavaScript_append_host();
    setTimeout(checkElement_append_host, 100);
  } else {
    setTimeout(checkElement_append_host, 100); // Check again in 100 milliseconds
  }
}

// Initial check on document ready
$(document).ready(function() {
  checkElement_append_host();
});

// Function to apply JavaScript code
function applyJavaScript_append_city() {
  $("#append_city").on("input", function() {
    var value = $(this).val();
    $(this).val(value.replace(/[^a-zA-Z0-9_\\. -]/g, ""));
  });
}

// Check if the textInput element is available
function checkElement_append_city() {
  if ($("#append_city").length) {
    applyJavaScript_append_city();
    setTimeout(checkElement_append_city, 100);
  } else {
    setTimeout(checkElement_append_city, 100); // Check again in 100 milliseconds
  }
}

// Initial check on document ready
$(document).ready(function() {
  checkElement_append_city();
});


// Function to apply JavaScript code
function applyJavaScript_append_host_multi() {
  $("#append_host_multi").on("input", function() {
    var value = $(this).val();
    $(this).val(value.replace(/[^a-zA-Z0-9_\\. -]/g, ""));
  });
}

// Check if the textInput element is available
function checkElement_append_host_multi() {
  if ($("#append_host_multi").length) {
    applyJavaScript_append_host_multi();
    setTimeout(checkElement_append_host_multi, 100);
  } else {
    setTimeout(checkElement_append_host_multi, 100); // Check again in 100 milliseconds
  }
}

// Initial check on document ready
$(document).ready(function() {
  checkElement_append_host_multi();
});

// Function to apply JavaScript code
function applyJavaScript_append_city_multi() {
  $("#append_city_multi").on("input", function() {
    var value = $(this).val();
    $(this).val(value.replace(/[^a-zA-Z0-9_\\. -]/g, ""));
  }); 
}

// Check if the textInput element is available
function checkElement_append_city_multi() {
  if ($("#append_city_multi").length) {
    applyJavaScript_append_city_multi();
    setTimeout(checkElement_append_city_multi, 100);
  } else {
    setTimeout(checkElement_append_city_multi, 100); // Check again in 100 milliseconds
  }
}

// Initial check on document ready
$(document).ready(function() {
  checkElement_append_city_multi();
});

// Function to apply JavaScript code
function applyJavaScript_new_var_name() {
  $("#new_var_name").on("input", function() {
    var value = $(this).val();
    $(this).val(value.replace(/[^a-zA-Z0-9_\\. -]/g, ""));
  }); 
}

// Check if the textInput element is available
function checkElement_new_var_name() {
  if ($("#new_var_name").length) {
    applyJavaScript_new_var_name();
    setTimeout(checkElement_new_var_name, 100);
  } else {
    setTimeout(checkElement_new_var_name, 100); // Check again in 100 milliseconds
  }
}

// Initial check on document ready
$(document).ready(function() {
  checkElement_new_var_name();
});
