<template>
  <el-container>
    <jq-dialog :title="title"
               :visible.sync="open"
               width="400px"
               append-to-body
               v-loading="loading"
               @close="$emit('update:show', false)"
               element-loading-text="系统拼命导入中"
               element-loading-spinner="el-icon-loading"
               element-loading-background="rgba(0, 0, 0, 0.8)">
      <el-upload
        ref="upload"
        :limit="1"
        :http-request="uploadRequest"
        accept=".zip, .rar"
        :headers="upload.headers"
        :action="upload.url + '?updateSupport=' + upload.updateSupport"
        :disabled="upload.isUploading"
        :auto-upload="false"
        drag
      >
        <i class="el-icon-upload"></i>
        <div class="el-upload__text">
          将文件拖到此处，或
          <em>点击上传</em>
        </div>
        <div class="el-upload__tip" slot="tip">
          <!--<el-link type="info" style="font-size:12px" @click="downLoadImportTemplate">{{ importFileName }} - 下载模板
          </el-link>-->
        </div>
        <div class="el-upload__tip" style="color:red" slot="tip">提示：仅允许导入“zip”或“rar”格式文件！</div>
      </el-upload>
      <div slot="footer" >
        <el-button type="primary" @click="submitFileForm">导 入</el-button>
        <el-button @click="cancelImport">关 闭</el-button>
      </div>
    </jq-dialog>


    <jq-dialog
      :title="importResultTitle"
      :visible.sync="importResultOpen"
      width="30%"
    >
      <span v-if="!importResult">导入失败: {{ faildNum }} 条数据异常</span>
      <div v-if="!importResult">
        <ul>
          <li v-for="item in errorData">
            {{ item }}
          </li>
        </ul>
      </div>
      <span v-if="importResult">导入成功: {{ successNum }} 条数据</span>
      <span slot="footer" class="dialog-footer">
    <el-button @click="importResultOpen = false">关 闭</el-button>
  </span>
    </jq-dialog>
  </el-container>

</template>

<script>
import {getToken} from "@/utils/auth";
import axios from "axios";

export default {
  name: "ImportZipDialog",
  data() {
    return {
      loading: false,
      importResult: true,
      successNum: 0,
      faildNum: 0,
      errorData: [],
      open: this.show,
      //导入结果弹窗
      importResultTitle: "导入结果",
      //导入结果弹窗
      importResultOpen: false,
      // 用户导入参数
      upload: {
        // 是否禁用上传
        isUploading: false,
        // 设置上传的请求头部
        headers: {Authorization: "Bearer " + getToken()},
        // 上传的地址
        url: process.env.VUE_APP_BASE_API + this.url
      },
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
    importFileName: {
      type: String
    },
    url:{
      type: String
    }
  },
  watch: {
    show () {
      this.open = this.show;
    }
  },
  methods: {
    // 提交上传文件
    submitFileForm() {
      this.$refs.upload.submit();
    },
    //取消导入
    cancelImport() {
      this.$emit('update:show', false)
    },

    //下载导入模板
    downLoadImportTemplate() {
      window.open(process.env.VUE_APP_BASE_API + "/common/download/template?fileName=" + encodeURI(this.importFileName) + "&type=import");
    },
    uploadRequest(param) {
      this.loading = true;
      let that = this
      let fileObj = param.file;
      let formData = new FormData();
      formData.append('file', fileObj);
      axios.post(
        process.env.VUE_APP_BASE_API +  this.url,
        formData,
        {
          headers: {Authorization: "Bearer " + getToken()},
        }
      ).then((res) => {
        this.importResultOpen = true;
        if (res.data.faildNum > 0) {
          this.importResult = false;
        }else{
          //调用父页面回调函数
          this.$emit("handleQuery");
          this.$emit('update:show', false)
        }
        this.successNum = res.data.successNum;
        this.errorData = res.data.errorData;
        this.faildNum = res.data.faildNum;
        this.loading = false;
      })
    },

  }
}
</script>

<style scoped>

</style>
