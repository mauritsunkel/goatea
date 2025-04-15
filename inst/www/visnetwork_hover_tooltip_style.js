document.addEventListener('DOMContentLoaded', function () {
    // Delay ensures visNetwork has rendered
    setTimeout(function () {
      const style = document.createElement('style');
      style.innerHTML = `
        #graphvno_ppi_visnetwork > div:nth-child(2),
        #graphvno_ppi_visnetwork_subgraph > div:nth-child(2) {
          background-color: black !important; 
          color: white !important; 
        }
      `;
      document.head.appendChild(style);
    }, 500); // wait 500ms just to be safe
  });