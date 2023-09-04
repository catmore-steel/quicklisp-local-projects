<template>

  <el-dialog
    v-el-drag-dialog
    :visible.sync="show"
    custom-class="file-view"
    style="z-index:998"
    width="37%"
    :modal="false"
    :center="true"
    :show-close="false"
    :append-to-body="true"
    :destroy-on-close="false">
    <template slot="title">
      <div class="file-view-header">
        <div style="float: left;text-align: left;">{{ '【' + fileNameView + '】' }}</div>
        <div style="position:absolute;right: 0;text-align: right;">
          <el-tooltip effect="dark" content="重命名" placement="top-start">
            <el-button size="mini" type="primary" icon="el-icon-edit" @click="resetNameOpen()"></el-button>
          </el-tooltip>
          <el-tooltip effect="dark" content="编辑文件" placement="top-start">
            <el-button type="primary" icon="el-icon-edit" @click="editFile(downloadUrl,fileName)"></el-button>
          </el-tooltip>
          <el-tooltip effect="dark" content="下载源文件" placement="top-start">
            <el-button type="primary" icon="el-icon-download" @click="downLoadFile(downloadUrl,fileName)"></el-button>
          </el-tooltip>
          <el-tooltip effect="dark" content="全屏预览" placement="top-start">
            <button class="el-button el-button--mini" style="background-color:#13ce66;border-color:#13ce66" @click="openNewTargt()">
              <i class="el-icon-news" style="color: #fff"></i>
            </button>
          </el-tooltip>
          <el-tooltip effect="dark" content="关闭" placement="top-start">
            <button class="el-button el-button--mini" style="background-color:#909399;border-color:#909399" @click="handleClose()">
              <i class="el-icon-close" style="color: #fff"></i>
            </button>
          </el-tooltip>
        </div>
        <div style="width: 300px;position:absolute;right: 20%;top: 8%">
          <el-popover placement="bottom" title="重命名" width="300" trigger="manual" v-model="visible">
            <div style="text-align: right; margin: 0">
              <el-form :disabled="false">
                <el-form-item>
                  <el-input type="text" v-model="inputName"/>
                  <div style="margin-top: 10px">
                    <el-button type="primary" @click="resetName(id,inputName)" :disabled="false">确定</el-button>
                    <el-button type="text" @click="cancel()" :disabled="false">取消</el-button>
                  </div>
                </el-form-item>
              </el-form>
            </div>
          </el-popover>
        </div>
      </div>
    </template>
    <div class="file-view-content" style="width: 100%;height:calc(100% - 52px);position: absolute;">
      <iframe style="width: 100%;height:97%" id="Iframe" :src="viewUrl"></iframe>
    </div>

  </el-dialog>
</template>

<script>

import elDragDialog from '@/utils/el-drag-dialog'
import axios from 'axios'
import {resetName} from "@/api/common";

const Base64 = require('js-base64').Base64
export default {
  name: 'JqFileView',
  directives: {elDragDialog},
  data() {
    return {
      inputName: null,
      visible: false,
      //正式环境
      // kkfile: 'http://220.178.164.67:8012',
      //本地测试环境
      kkfile: 'http://192.168.1.22:8012',
      jqOffice: 'http://220.178.166.133:82/api/edit',
      viewUrl: null,
      downloadUrl: '',
      fileNameView: '',
      form: {
        id: null,
        fileOrgName: null
      }
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
    attachFlg:{  //非公共附件类型预览
      type: Boolean,
      default: true
    },
    downUrl:{ // 非公共附件类型预览需要直接传递拼接好的URL
      type: String,
      default: ""
    },
    id: {  //公共附件预览
      type: Number,
      default: null
    },

    folder: {
      type: String,
      default: ''
    },
    uploadType: {
      type: String,
      default: ''
    },
    fileName: {
      type: String,
      default: ''
    },
    location: {
      type: String,
      default: '1'
    }
  },
  watch: {
    changeData(){
      this.fileNameView = this.fileName
      const apiHostPath = this.$store.getters.apiHostPath;

      let officePreviewType = this.fileName.substring(this.fileName.lastIndexOf('.') + 1) // 后缀名
      this.downloadUrl = apiHostPath + this.folder + this.id + '/' + new Date().getTime() + '.' + officePreviewType
      if (officePreviewType == 'pdf') {
        if (this.attachFlg){ //公共附件表 预览PDF
          this.viewUrl = "/pdf/web/viewer.html?file=" + encodeURIComponent(this.downloadUrl);
        }else{
          this.viewUrl = "/pdf/web/viewer.html?file=" + encodeURIComponent(this.downUrl);
        }
      } else if (officePreviewType == 'docx') {
        if (this.attachFlg){ //公共附件表 预览Docx
          this.viewUrl = this.kkfile + '/onlinePreview?url=' + encodeURIComponent(Base64.encode(this.downloadUrl))
          let width = window.screen.width;
          let height = window.screen.height;
          const apiHostPath = this.$store.getters.apiHostPath;
          let docUrl = apiHostPath + '/file/attach/common?attachId=' + this.id;
          if (this.show){
            POBrowser.openWindowModeless(docUrl, 'width=' + width + ';height=' + height + ';')
          }
        }else{
          this.viewUrl = this.kkfile + '/onlinePreview?url=' + encodeURIComponent(Base64.encode(this.downUrl))
        }
      } else {
        if (apiHostPath.includes('http://192.168.', 0) || apiHostPath.includes('http://127.0.0.1', 0) || apiHostPath.includes('http://localhost', 0)) {
          // this.msgError('KKfile不支持本地IP预览！')
        }
        if (this.attachFlg){ //公共附件表 预览其他类型文件
          this.viewUrl = this.kkfile + '/onlinePreview?url=' + encodeURIComponent(Base64.encode(this.downloadUrl))
        }else{
          this.viewUrl = this.kkfile + '/onlinePreview?url=' + encodeURIComponent(Base64.encode(this.downUrl))
        }
      }
    },
  },
  computed: {
    changeData(){
      const {id , downUrl , show} = this
       return {id , downUrl , show}
    }
  },
  created() {
    this.fileNameView = this.fileName
  },
  methods: {
    resetNameOpen() {
      this.visible = true;
      this.inputName = this.fileNameView.substring(0, this.fileName.lastIndexOf('.')) // 后缀名
      this.$emit('reName', false)
    },
    cancel() {
      this.visible = false;
      this.inputName = null;
    },
    // 点击关闭 jqcard 弹窗
    handleClose() {
      this.$emit('update:show', false)
    },
    downLoadFile(srcUrl, fileName) {
      if (this.attachFlg){
        window.open(srcUrl, '_blank')
      }else{
        window.open(this.downUrl, '_blank')
      }
    },
    editFile(srcUrl, fileName) {
      this.viewUrl = "";
      let width = window.screen.width;
      let height = window.screen.height;
      const apiHostPath = this.$store.getters.apiHostPath;
      let docUrl = apiHostPath + '/file/attach/common?attachId=' + this.id;
      POBrowser.openWindowModeless(docUrl, 'width=' + width + ';height=' + height + ';')
      // const apiHostPath = this.$store.getters.apiHostPath;
      // let callbackUrl = apiHostPath + '/common/edit'
      //
      // let userId = this.$store.state.user.userId
      // let userName = this.$store.state.user.nickName
      // let userGroup = this.$store.state.user.deptName
      // let officeUrl = this.jqOffice + '?url=' + Base64.encode(srcUrl) + '&callbackUrl=' + Base64.encode(callbackUrl)
      //   + '&fileId=' + this.id + '&userId=' + userId + '&userName=' + userName + '&userGroup=' + userGroup + '&title=' + fileName
      // window.open(officeUrl, '_blank')
    },
    openNewTargt() {
      window.open(this.viewUrl, '_blank')
    },
    resetName(id, inputName) {
      this.form.id = id
      this.form.fileOrgName = inputName + this.fileName.substring(this.fileName.lastIndexOf('.')) // 后缀名
      // console.log(this.form)
      resetName(this.form).then(response => {
        if (response.code === 200) {
          this.msgSuccess(response.msg)
          this.fileNameView = this.form.fileOrgName;
          this.inputName = inputName;
          let name = this.form.fileOrgName;
          this.$emit('reName', this.form.id, this.form.fileOrgName)
          this.visible = false
        }
      })
    }
  }

}
</script>

<style scoped lang="scss">
/deep/ .file-view , .el-dialog__wrapper{
  pointer-events: none;
}
/deep/ .file-view , .el-dialog:not(.is-fullscreen) {
  transform : none;
}
/deep/ .file-view, .el-dialog {
  top:0;
  margin: 0;
  height: 100vh;
  pointer-events:auto;
  resize:both;
  overflow:auto;
  border: 4px solid #304156;
  color: white;
  background-color: #304156;

  .file-view-header {
    position: relative;
    line-height: 32px;
    height: 32px;
  }

  .el-dialog__header {
    padding: 10px;

  }

  .el-dialog--center .el-dialog__body {
    padding: 0px !important;
  }

  .el-dialog__body {
    min-height: 100px;
    padding: 0px !important;
  }

}

</style>

