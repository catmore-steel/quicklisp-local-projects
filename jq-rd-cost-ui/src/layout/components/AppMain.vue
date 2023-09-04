<template>
  <section class="app-main">
    <transition name="fade-transform" mode="out-in">
      <keep-alive :include="cachedViews">
        <router-view :key="key" />
      </keep-alive>
    </transition>
    <!-- 许可证 -->
    <div class="license_mask" v-if="isLicense">
      <div class="license">
        <div class="head">
          <div style="padding-top: 120px;width: 600px;">{{ licenseObj.licInfo.extra.companyName || '-' }} | {{ licenseObj.licInfo.extra.creditCode || '-' }}</div>
        </div>
        <div class="body">
          <div class="os_info">
            <div class="title">我的系统信息 <i class="el-icon-document-copy" @click="copyNodeValue($event,'devInfo')"></i>
            </div>
            <div class="grid_box">
              <div t>CPU序列号:</div>
              <div t>主板序列号:</div>
              <div>{{ licenseObj.devInfo.cpuSerial || '-' }}</div>
              <div>{{ licenseObj.devInfo.mainBoardSerial || '-' }}</div>
              <div t>当前申报项目数:</div>
              <div t>当前用户人数:</div>
              <div>{{ licenseObj.nowBusAmount || '-' }}</div>
              <div>{{ licenseObj.nowConsumerAmount || '-' }}</div>
              <div t>MAC地址:</div>
              <div t>IP地址:</div>
              <div>
                <div v-for="item in licenseObj.devInfo.macAddress">{{ item }}</div>
              </div>
              <div v-if="licenseObj.devInfo.macAddress.length == 0">-</div>
              <div>
                <div v-for="item in licenseObj.devInfo.ipAddress">{{ item }}</div>
              </div>
              <div v-if="licenseObj.devInfo.ipAddress.length == 0">-</div>
            </div>
          </div>
          <div class="active_info">
            <div class="title">当前许可证信息 <i class="el-icon-document-copy" @click="copyNodeValue($event,'licInfo')"></i>
            </div>
            <div>
              <div t>MAC地址:</div>
              <div>
                <div v-for="item in licenseObj.licInfo.extra.macAddress">{{ item }}</div>
              </div>
              <div v-if="licenseObj.licInfo.extra.macAddress.length == 0">-</div>
              <div t>限制申报项目数:</div>
              <div>{{ licenseObj.licInfo.extra.busAmount || '-' }}</div>
              <div t>限制用户人数:</div>
              <div>{{ licenseObj.licInfo.extra.consumerAmount || '-' }}</div>
              <div t>有效期开始截止日期:</div>
              <div v-if="licenseObj.licInfo.notBefore">
                {{ formatDate(licenseObj.licInfo.notBefore) || '-' }}
                至
                {{ formatDate(licenseObj.licInfo.notAfter) || '-' }}
              </div>
              <div v-else>-</div>
            </div>
          </div>
          <div class="up_load">
            <div style="margin: 20px 0">许可证: <span style="color: #02a7f0">license.lic</span></div>
            <el-button type="primary" @click="$refs.file.click()">上传新许可证</el-button>
            <input ref="file" style="display: none;" type="file" @change="uploadRequest($event)">
            <div style="color: #f56c6c;margin-top: 10px;">{{ msg }}</div>
          </div>
          <div v-if="licenseObj.verify == false" class="help" style="color: #d9001b">
            尊敬的用户，您的许可证已到期，如需继续使用请及时联系客服人员（400-******）,更新许可证。
          </div>
          <div v-else class="help">如需更多服务或帮助，请联系客服电话（400-******）</div>
        </div>
        <div class="footer">
          <el-button v-if="licenseObj.verify == false" @click="loginOut">返回登录</el-button>
          <el-button v-else @click="isLicense = false">返 回</el-button>
        </div>
      </div>
    </div>
  </section>
</template>

<script>
import { uploadLicense } from '@/api/system/license'
import Clipboard from 'clipboard'

export default {
  name: 'AppMain',
  data(){
    return {
      licenseObj: {},
      isLicense : false,
      msg:""
    }
  },
  created() {
    this.$store.dispatch('app/getLicenseInfo').then(data => {
      this.isLicense = !data.verify
      this.licenseObj = data
      localStorage.setItem('creditCode', data.licInfo.extra.creditCode);
    })
  },
  mounted() {
    this.EventBus.$on('openLicBox',()=>{
      this.isLicense = true
    })
  },
  methods:{
    copyNodeValue(e,type) {
      const clipboard = new Clipboard(e.target, { text: () => JSON.stringify(this.licenseObj[type])})
      clipboard.on('success', e => {
        this.$message.success('复制成功!')
        // 释放内存
        clipboard.off('error')
        clipboard.off('success')
        clipboard.destroy()
      })
      // err
      clipboard.on('error', e => {
        this.$message.error('复制失败!')
        clipboard.off('error')
        clipboard.off('success')
        clipboard.destroy()
      })
      clipboard.onClick(e)
    },
    formatDate(time){
      let date = new Date(time)
      let year = date.getFullYear()
      let month = date.getMonth() + 1 < 10 ? '0' + (date.getMonth() + 1) : date.getMonth() + 1
      let day = date.getDate() < 10 ? '0' + date.getDate() : date.getDate()
      return `${year}-${month}-${day}`
    },

    /** 上传文件 */
    uploadRequest(param) {
      param.preventDefault()
      const files = this.$refs.file.files
      const data = new FormData()
      for(const file of files){
        data.append('file', file)
      }
      uploadLicense(data).then(res => {
        this.$router.go(0)
      }).catch(err => {
        this.msg = "校验不通过"
        this.$refs.file.value = null;
      })
    },

    loginOut(){
      this.$store.dispatch('LogOut').then(() => {
        location.href = '/index';
      })
    }
  },
  computed: {
    cachedViews() {
      return this.$store.state.tagsView.cachedViews
    },
    key() {
      return this.$route.path
    }
  }
}
</script>

<style lang="scss" scoped>
.app-main {
  /* 50= navbar  50  */
  min-height: calc(100vh - 50px);
  width: 100%;
  position: relative;
  overflow: auto;
}

.fixed-header+.app-main {
  padding-top: 50px;
}

.hasTagsView {
  .app-main {
    /* 84 = navbar + tags-view = 50 + 34 */
    min-height: calc(100vh - 84px);
    overflow: auto;
  }

  .fixed-header+.app-main {
    padding-top: 84px;
  }
}

.license_mask{
  position: absolute;
  top: 0;
  left: 0;
  width: 100%;
  height: 100%;
  z-index: 888;
  background-color: rgba(0,0,0,.3);
}

.license{
  position: absolute;
  top: 5%;
  left: 15%;
  z-index: 999;
  width: 70%;
  height: 70%;
  background-color: #fff;
  border-radius: 10px;
  overflow: hidden;
  box-shadow: 10px 10px 20px #888;
  text-align: center;
  .head{
    height: 35%;
    background-image: url("../../assets/image/license.png") ;
    background-size: cover;
    h2{
      position: absolute;
      left: 40%;
      display: inline-block;
      align-items: center;
      top: 27%;
      text-align: center;
      margin: 0;
    }
  }
  .body{
    padding-top: 20px;
    text-align: left;
    height: 55%;
    display: flex;
    justify-content: space-around;
    line-height: 26px;
    .title{
      font-size: 18px;
      font-weight: 600;
      color: #333;
      margin-bottom: 15px;
      i{
        font-weight: 600;
        cursor: pointer;
      }
      i:hover{
        color: #1393f5;
      }
    }
    .help{
      color: #aaa;
      position: absolute;
      top: calc(90% - 20px);
    }
    .active_info{
      div[t]{
        color: #7f7f7f;
      }
    }
    .grid_box{
      display: grid;
      grid-column-gap: 20px;
      grid-template-rows: repeat(5,auto);
      grid-template-columns: repeat(2,auto);
      div[t]{
        color: #7f7f7f;
      }
    }

  }
  .footer{
    display: flex;
    justify-content: center;
    align-items: center;
    height: 10%;
  }
}
</style>

<style lang="scss">
// fix css style bug in open el-dialog
.el-popup-parent--hidden {
  .fixed-header {
    padding-right: 15px;
  }
}
</style>
