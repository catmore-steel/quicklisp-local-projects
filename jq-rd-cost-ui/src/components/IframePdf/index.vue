<template>
  <div>
    <iframe :src="pdfUrl"
            width="100%"
            :height="height+'px'"
    ></iframe>
  </div>


</template>


<script>
  export default {
    name: "ReadPdf",
    data() {
      return {
        pdfUrl: null,
      }
    },
    props: {
      title: {
        type: String,
        default: '导入'
      },
      fileName: {
        type: String,
        default: ''
      },
      height:{
        type: Number,
        default: 500
      },
      location: {
        type: String,
        default: ''
      },
      applyType: {
        type: String,
        default: ''
      },
    },
    watch: {
      location() {
        this.getPdfUrl();
      },
    },
    created() {
      this.getPdfUrl()
    },
    methods:{
      getPdfUrl(){
        const head =  process.env.VUE_APP_BASE_API ;
        this.pdfUrl = "/pdf/web/viewer.html?file=" + encodeURIComponent(head + "/common/download/applyFile?applyType=" + this.applyType + "&location=" + this.location + "&fileName=" + this.fileName + ".pdf");
      }
    },
  }
</script>

<style scoped>
  .pdf-dialog >>> .el-dialog__body{
    padding:0px !important;
  }

  .pdf-dialog >>> .el-dialog__header{
    background-image: linear-gradient(to right, #292929 0%, #ffffff 100% );
  }
  .pdf-dialog >>> .el-dialog__title{
    color: rgb(24, 144, 255);
  }
</style>
