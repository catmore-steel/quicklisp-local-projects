<template>
  <div class="jq-read-pdf">
    <jq-dialog
      v-el-drag-dialog
      :title="title"
      class="pdf-dialog"
      :visible.sync="open"
      :modal="false"
      fullscreen
      append-to-body
      @close="$emit('update:show', false)"
      element-loading-background="rgba(0, 0, 0, 0.8)">
      <iframe :src="pdfUrl"
              id="readPdfId"
              width="100%"
              height="740px"
      >
      </iframe>
    </jq-dialog>
  </div>
</template>


<script>
import elDragDialog from "@/utils/el-drag-dialog";

export default {
  directives: {elDragDialog},
  name: "ReadPdf",
  data() {
    return {
      open: this.show,
      pdfUrl: null,
    }
  },
  props: {
    title: {
      type: String,
      default: '导入'
    },
    show: {
      type: Boolean,
      default: false
    },
    fileName: {
      type: String,
      default: ''
    },
    location: {
      type: String,
      default: ''
    },
  },
  watch: {
    show() {
      this.getPdfUrl();
      this.open = this.show;
    }
  },
  created() {
    // let target = document.querySelector('#readPdfId')
    // target.setAttribute("height","calc( 100% - 150px)");
  },
  methods: {
    getPdfUrl() {
      const apiHostPath = this.$store.getters.apiHostPath;
      this.pdfUrl = "/pdf/web/viewer.html?file=" + encodeURIComponent(apiHostPath + this.location);
    }
  },
}
</script>

<style lang="scss">

.jq-read-pdf {
  .pdf-dialog {
    .el-dialog {
      transform: none !important;
    }
  }

  .pdf-dialog > > > .el-dialog__body {
    padding: 0px !important;
  }

  .pdf-dialog > > > .el-dialog__header {
    background-image: linear-gradient(to right, #292929 0%, #ffffff 100%);
  }

  .pdf-dialog > > > .el-dialog__title {
    color: rgb(24, 144, 255);
  }

  .jq_dialog .container .jq_dialog_body {
    padding: 0px !important;
  }
}

</style>
