<template>
  <transition name="dialog-fade"  >
    <div class="jq_dialog" v-if="visible" @click.self="handleWrapperClick">
      <div class="container" :style="{width: width ||   '',height:height || '',top:top||''}" :class="fullscreen ? 'fullscreen' : ''">
        <div class="jq_dialog_header">
          <slot name="title">
            <span>{{ title }}</span>
          </slot>
          <i @click="handleClose" class="el-icon-close"></i>
        </div>
        <div class="jq_dialog_body" v-if="rendered">
          <slot></slot>
        </div>
        <div class="jq_dialog_footer" v-if="$slots.footer">
          <slot name="footer"></slot>
        </div>
      </div>
    </div>
  </transition>

</template>

<script>
import { isNullOrEmpty } from '@/utils/jq'

export default {
  name: 'JqDialog',
  props: {
    title: {
      type: String,
      default: ''
    },
    visible: {
      type: Boolean,
      default: false
    },
    width: {
      type: String,
      default: ''
    },
    height: {
      type: String,
      default: ''
    },
    top: {
      type: String,
      default: ''
    },
    // 通过点击遮罩层关闭dialog
    closeOnClickModal: {
      type: Boolean,
      default: false
    },
    // 点击esc关闭弹窗
    closeOnPressEscape: {
      type: Boolean,
      default: true
    },
    // 关闭dialog之前的回调函数
    beforeClose: Function,
    // 全屏
    fullscreen:{
      type: Boolean,
      default: false
    },
    appendToBody: {
      type: Boolean,
      default: false
    }
  },
  data() {
    return {
      rendered: false
    }
  },
  watch: {
    visible(val) {
      if (!val) {
        //document.body.appendChild(this.$el);
        window.removeEventListener('keydown', this.ESCclose)
      } else {
        // 监听esc，关闭dialog
        if (this.closeOnPressEscape) {
          window.addEventListener('keydown', this.ESCclose)
        }
      }
    }
  },
  methods: {
    handleClose() {
      if (typeof this.beforeClose === 'function') {
        this.beforeClose(this.hide)
      } else {
        this.hide()
      }
    },
    hide() {
      this.$emit('update:visible', false)
      this.$emit('close')
    },
    // 点击遮罩层
    handleWrapperClick() {
      if (!this.closeOnClickModal) return
      this.handleClose()
    },
    ESCclose(event) {
      if (event.keyCode === 27) {
        this.handleClose()
      }
    }
  },
  mounted() {
    this.rendered = true
    if (this.appendToBody) {
      if(!isNullOrEmpty(document.querySelector(".app-container"))){
        document.querySelector(".app-container").appendChild(this.$el);
      }
    }
  }
}
</script>

<style scoped lang="scss">
.jq_dialog {
  z-index: 997;
  position: absolute;
  top: 0;
  right: 0;
  bottom: 0;
  left: 0;
  overflow: auto;
  background-color: rgba(0, 0, 0, .4);




  .container {
    height: auto;
    position: relative;
    /*border-radius: 20px;*/
    margin: 8% auto 0 auto;
    width: 600px;
    background-color: #fff;

    .jq_dialog_header {
      border-radius: 5px;
      padding-left: 15px;
      height: 50px;
      font-size: 16px;
      background: #eeeeef;

      span {
        line-height: 50px;
        font-size: 18px;
        color: #303133;
      }

      i {
        position: absolute;
        top: 20px;
        right: 20px;
        cursor: pointer;
        font-size: 20px;
      }

      i:hover {
        color: #409eff;
      }
    }

    .jq_dialog_body {
      padding: 20px;
      color: #606266;
    }


    .jq_dialog_footer{
      border-radius: 5px;
      background-color: #eeeeef;
      width: 100%;
      position: sticky;
      bottom: 1px;
      text-align: center;
      padding: 10px 0;
    }


  }

  .fullscreen{
    overflow: auto;
    border-radius: 0;
    margin: 0;
    width: 100%;
    height: 100%;
    .jq_dialog_header{
      //height: 7%;
    }
    .jq_dialog_body{
      height: calc(92% - 60px);
      overflow-y: auto;
      //min-height: 85%;
    }
    .jq_dialog_footer{
      display: flex;
      align-items: center;
      justify-content: center;
      height: 8%;
      background-color: #eeeeef;
      width: 100%;
      position: sticky;
      bottom: 0;
    }

  }
}
</style>
