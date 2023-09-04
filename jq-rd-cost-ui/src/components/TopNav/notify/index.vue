<template>
  <div :style="{right: show ? '16px' :'-290px'}" class="notify" draggable @click="show = !show">
    <div class="title">{{ infoObj.title }}</div>
    <div class="left">
      <i :class="type[infoObj.code].icon"
         :style="{color: type[infoObj.code].color}"
      ></i>
    </div>
    <div class="right">
      <div class="title">{{ infoObj.msg }}</div>
      <div class="content">
        <strong>
          <i>
            {{ infoObj.data }}
          </i>
        </strong>
      </div>
    </div>
  </div>
</template>

<script>
export default {
  name: 'notify',
  data() {
    return {
      show: false,
      infoObj: {
        code :'0'
      },
      type: {
        '0': { icon: 'el-icon-info', color: '#909399' },
        '1': { icon: 'el-icon-success', color: '#67c139' },
        '200': { icon: 'el-icon-loading', color: '#67c139' },
        '500': { icon: 'el-icon-error', color: '#f56c6c' }
      }
    }
  },
  created() {
    this.initSocket()
  },
  methods: {
    initSocket() {
      let {port,host} = window.location;
      let url ="ws://" + host
      // 前端在80，port 为 ""
      if(port == ""){
        url+=":8071"
      }else{
        url = url.replace(port,"8071")
      }
      // 长连接 是否有提交材料任务在执行
      let ws = new WebSocket(url + '/websocket/' + this.$store.getters.userId)
      ws.onopen = function() {
        console.log('socket服务器连接成功...')
      }
      const _this = this
      ws.onmessage = function({ data }) {
        _this.infoObj = eval('(' + data + ')')
        // 如果暂无任务显示超过十秒 就给他隐藏了
        if(_this.infoObj.code == '0'){
          setTimeout(()=>{
            _this.show = false
          },10000)
        }
      }
    }
  }
}
</script>

<style lang="scss" scoped>
.notify {
  cursor: pointer;
  transition: all .4s ease;
  z-index: 666;
  display: flex;
  width: 330px;
  padding: 14px 26px 14px 13px;
  border-radius: 8px;
  box-sizing: border-box;
  border: 1px solid #e6ebf5;
  position: fixed;
  background-color: #FFFFFF;
  box-shadow: 0 2px 12px 0 rgba(0, 0, 0, .1);
  bottom: 35px;

  .left {
    width: 30px;

    i {
      color: #fff;
      font-size: 24px;
    }
  }

  .right {
    margin-left: 13px;
    margin-right: 8px;

    .title {
      height: 24px;
      line-height: 24px;
      font-weight: 700;
      font-size: 16px;
      color: #303133;
    }

    .content {
      font-size: 14px;
      line-height: 21px;
      margin: 6px 0 0;
      color: #606266;
      word-break: break-all;
    }
  }
}
</style>
