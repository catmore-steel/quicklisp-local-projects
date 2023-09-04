<template>
  <div id="app">
    <router-view />
  </div>
</template>

<script>
export default  {
  name:  'App',
  mounted() {
    const apiHostPath = this.$store.getters.apiHostPath;
    let element = document.createElement("script");
    element.type = 'text/javascript';
    element.src = apiHostPath + "/pageoffice.js";
    document.body.appendChild(element);
  },
}
</script>

<style>
  /* 滚动条宽高，横向滚动条的高，纵向滚动条的宽 */
  ::-webkit-scrollbar-track-piece{width:10px;border-right:1px solid #EEE;border-left:1px solid #e4e4e4;background-color:#f0f0f0;}
  ::-webkit-scrollbar{width:10px;height:10px}
  ::-webkit-scrollbar-thumb{position:relative;min-height:25px;background-color:#65e6ff;background-clip:padding-box}
  ::-webkit-scrollbar-thumb:hover{background-color: #ff6f6f;}
  ::-webkit-scrollbar-thumb:active{background-color:#62f556;}
  ::selection{background:#4792FF;color:#fff}
  ::-moz-selection{background:#4792FF;color:#fff}

</style>
