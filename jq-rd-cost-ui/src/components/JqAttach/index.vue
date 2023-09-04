<template>
  <div>
    <el-upload
      :disabled="inputDisabled"
      ref="upload"
      :multiple="upload.multiple"
      :limit="upload.limit"
      :on-exceed="onExceed"
      :action="upload.url"
      :headers="upload.headers"
      :list-type="listType"
      :file-list="upload.fileList"
      :on-progress="handleFileUploadProgress"
      :on-success="handleFileSuccess"
      :on-remove="handleFileRemove"
      :on-preview="handlePreview"
      :on-change="handleChangeFile"
      :auto-upload="true"
      :accept="accept"
      >
      <el-button slot="trigger" type="primary" :disabled="disabled">{{checkMany?'选取文件':'选取单文件'}}</el-button>
<!--      <span slot="tip" class="el-upload__tip" v-if="checkMany == false"> 仅支持单个文件上传,删除后重新上传!</span>-->
    </el-upload>
    <file-view :show.sync="viewFileShow" :folder="file.folder" :id="file.id" :location="file.location"
               :file-name.sync="file.fileName"
               :upload-type="file.uploadType" :input-name="file.inputName" @reName="reName"></file-view>
  </div>
</template>

<script>
import {getToken} from '@/utils/auth'
import axios from 'axios'
import FileView from '../JqFileView/index'

export default {
  components: {FileView},
  name: 'JqAttach',
  data() {
    return {
      srcUrl: null,
      open: this.show,
      viewFileShow: false,
      downloadUrl: null,
      file: {
        folder: '',
        uploadType: '1',
        fileName: '',
        location: '',
        inputName: '',
      },
      upload: {
        // 设置上传的请求头部
        headers: {Authorization: 'Bearer ' + getToken()},
        // 上传的地址
        url: process.env.VUE_APP_BASE_API + '/common/upload?relatePage=' + this.relatePage + '&relateKey=' + this.relateKey,
        // 上传的文件列表
        fileList: [],
        limit: 1,
        isUploading: false,
        multiple: false
      }
    }
  },
  inject: {
    elForm: {
      default: ''
    },
    elFormItem: {
      default: ''
    }
  },
  props: {
    value: {
      type: Array,
      default: () => []
    },
    relatePage: {
      type: String
    },
    relateKey: {
      type: String,
      default: 'fj1'
    },
    checkMany: {
      type: Boolean,
      default: true
    },
    // text/picture/picture-card
    listType: {
      type: String,
      default: 'text'
    },
    accept: {
      type: String,
      default: '*'
    },
    disabled: {
      type: Boolean,
      default: false
    }

  },
  computed: {
    inputDisabled() {
      return this.disabled || (this.elForm || {}).disabled
    }
  },
  created() {
    this.upload.fileList = this.value
    if (this.checkMany == true) {
      this.upload.limit = 99999
      this.upload.multiple = true
    } else {
      this.upload.limit = 1
      this.upload.multiple = false
    }

  },
  watch: {
    show() {
      this.open = this.show
    },
    // newVal 获取props中的值，方法名 = props中的参数
    value(newVal, oldVal) {
      this.upload.fileList = newVal
    },
    relatePage(newVal, oldVal){
      this.upload.url = process.env.VUE_APP_BASE_API + '/common/upload?relatePage=' + newVal + '&relateKey=' + this.relateKey
    }
  },
  methods: {
    // 文件上传中处理
    handleFileUploadProgress(event, file, fileList) {
      //处理动画
      this.upload.isUploading = true
    },
    // 文件上传成功处理
    handleFileSuccess(res, file, fileList) {
      // console.log(res);
      if (res.code == 200) {
        //关闭处理动画
        this.msgSuccess(res.msg)
        this.upload.isUploading = false
        this.upload.fileList.push(res.data)
        this.$emit('changeAttachmentList', this.upload.fileList)
      } else {
        this.msgError(res.msg)
      }
      // console.log( this.upload.fileList);
    },
    handleFileRemove(file, fileList) {
      this.upload.fileList = fileList
      this.$emit('changeAttachmentList', fileList)
    },
    handleChangeFile(file, fileList) {
      this.isDisabled = false
      if (!file) return
      var testmsg = file.name.substring(file.name.lastIndexOf('.') + 1)
      const isLt2M = file.size / 1024 / 1024 < 500
      // if (testmsg !== 'xls' && testmsg !== 'xlsx' && testmsg !== 'csv') {
      //   this.$refs.upload.clearFiles()
      //   this.$message({
      //     message: '上传文件只能是 .xls、.xlsx、csv格式!',
      //     type: 'warning'
      //   })
      //   return false
      // }
      if (!isLt2M) {
        this.$refs.upload.clearFiles()
        this.$message({
          showClose: true,
          message: '上传文件大小不能超过 500MB!',
          type: 'warning'
        })
        return false
      }
      this.fileName = file.name
    },
    onExceed(files, fileList) {
      this.$message({
        showClose: true,
        message: '仅支持单个文件上传,删除后重新上传!',
        type: 'warning'
      })
    },
    /**
     *  点击下载
     * @param file
     */
    handlePreview(file) {
      // console.log(file);

      if (file.status == 'uploading') {
        this.$message({
          showClose: true,
          message: '文件上传中无法预览或下载!',
          type: 'warning'
        })
        return
      }
      debugger
      this.file.id = file.id
      this.file.fileName = file.name
      this.file.uploadType = file.uploadType
      this.file.location = file.location
      this.file.inputName = file.name.substring(0, file.name.lastIndexOf('.'))
      this.file.folder = '/common/download/'
      this.viewFileShow = true
    },
    reName(fileId, newName) {
      this.upload.fileList.forEach(e => {
        if (e.id === fileId) {
          e.name = newName;
          e.fileOrgName = newName;
        }
      });
    }
  }
}
</script>

<style scoped lang="scss">

/deep/ .el-dialog--center .el-dialog__body {
  padding: 0px !important;
}

/deep/ .el-dialog__body {
  padding: 0px !important;
}
</style>
