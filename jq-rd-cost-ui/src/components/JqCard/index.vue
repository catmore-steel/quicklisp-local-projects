<template>
  <transition name="fade">
    <div class="jq_card" v-show="show" :style="'z-index:'+ zIndex">
      <div class="card_header" style="">
        <!--  card 的标题  -->
        <div :class="['card_header_title',!cardKey?'new':'detail']">
          <el-tooltip class="item" effect="dark" :content="title" placement="left">
            <div class="title-text">{{ title }}</div>
          </el-tooltip>
        </div>
        <div class="card_header_body">
          <div class="new-detail" v-if="!cardKey">
            <slot name="new"></slot>
          </div>
          <!-- 标签 -->
          <div class="tags" v-if="tags">
            <slot name="tags">
              <div v-for="(item ,index) in tags" :key="index" class="tag_item" :style="cmpWidth">
                <div class="label">{{ Object.keys(item)[0] }}</div>
                <el-tooltip class="item" effect="dark" content="复制" placement="top-start">
                  <div class="value" @click="copyNodeValue($event,item[Object.keys(item)[0]])">
                    {{ item[Object.keys(item)[0]] }}
                  </div>
                </el-tooltip>
              </div>
            </slot>
          </div>
          <!-- 预留的操作 -->
          <div class="operate" v-if="$slots.operate">
            <div class="operateBtn">
              <slot name="operate"></slot>
            </div>
          </div>
          <div class="operate" v-if="!$slots.operate && cardKey">
             <el-tag type="danger" style="margin-left: 10px">无可用操作</el-tag>
          </div>
        </div>

      </div>
      <!--  card 的主体  -->
      <div class="card_body" v-if="show">
        <slot></slot>
      </div>
      <!--  card 的底部  -->
      <div class="card_footer" v-if="$slots.footer">
        <slot name="footer"></slot>
      </div>
      <button class="close " @click="handleClose"><i class="el-icon-close"></i></button>
    </div>
  </transition>

</template>

<script>

import Clipboard from 'clipboard'

export default {
  name: 'JqCard',
  data() {
    return {
      zIndex:10,
    }
  },

  props: {
    tags: {
      type: Array
    },
    title: {
      type: String
    },
    cardKey: {
      type: [Number,String],
      require: true
    },
    show: {
      type: Boolean,
      default: false
    },
    // 点击esc关闭弹窗
    closeOnPressEscape: {
      type: Boolean,
      default: true
    },
  },
  computed: {
    cmpWidth() {
      return 'width:' + 100 / (this.tags.length) + '%'
    }
  },
  watch: {
    show(val) {
      if (val) {
        this.zIndex++
        // 监听esc，关闭dialog
        if (this.closeOnPressEscape) {
          window.addEventListener('keydown', this.ESCclose)
        }
      }
    }
  },
  methods: {
    // 点击关闭 jqcard 弹窗
    handleClose() {
      window.removeEventListener('keydown', this.ESCclose)
      this.$emit('handleClose')
    },

    ESCclose(event) {
      if (event.keyCode === 27) {
        this.handleClose()
      }
    },
    copyNodeValue(e, text) {
      const clipboard = new Clipboard(e.target, {text: () => text})
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
    }
  }
}
</script>

<style lang="scss">
.jq_card {
  line-height: normal;
  font-family: "Helvetica Neue",Helvetica,"PingFang SC","Hiragino Sans GB","Microsoft YaHei","微软雅黑",Arial,sans-serif;
  position: fixed;
  z-index: 10;
  padding: 0px 20px;
  height: 100vh;
  width: 60vw;
  top: 0;
  right: 0;
  background-color: #fcfcfc;
  //backdrop-filter: blur(70px);
  box-shadow: -10px 0 30px 0 #eee;

  .card_header {
    height: 120px;
    width: 100%;
    margin-top: 14px;

    .card_header_title {
      position: relative;
      float: left;
      padding: 10px;

      color: #fff;

      border-radius: 3px;
      margin-right: 10px;
      height: 100%;

      .title-text {
        font-size: 16px;
        width: 16px;
        height: 90px;
        align-items: center;
        flex: 1;
        overflow: hidden;
        -webkit-flex: 1;
        display: flex;
        text-align: center;
      }
    }

    .new {
      background-color: #1890ff;
      border-color: #1890ff;
    }

    .detail {
      background-color: #42b983;
      border-color: #42b983;
    }

    .card_header_body {

      width: calc(100% - 46px);
      float: right;
      height: 100%;

      .operate {
        padding-left: 10px;
        height: 40px;
        margin-top: 8px;
        border: 1px solid #e1e1e1;
        text-align: center;
        font-size: 14px;
        background-color: #f4f5f7;
        display: -webkit-box;
        display: -ms-flexbox;
        -webkit-box-pack: justify;
        -ms-flex-pack: justify;
        justify-content: space-between;
        border-radius: 3px;
        line-height: 38px;
      }
      .operateBtn{
        .el-button{
          margin-left: 10px;
        }

        .el-button + .el-button {
          margin-left: 10px;
        }

      }
    }
  }

  .new-detail {
    height: 118px;
    border: 1px solid #e1e1e1;
    text-align: center;
    font-size: 14px;
    background-color: #f4f5f7;
    display: -webkit-box;
    display: -ms-flexbox;
    display: flex;
    -webkit-box-pack: justify;
    -ms-flex-pack: justify;
    justify-content: space-between;
    padding: 10px;
    border-radius: 3px;
  }

  .tags {
    height: 70px;
    border: 1px solid #e1e1e1;
    text-align: center;
    font-size: 14px;
    background-color: #f4f5f7;
    display: -webkit-box;
    display: -ms-flexbox;
    display: flex;
    -webkit-box-pack: justify;
    -ms-flex-pack: justify;
    justify-content: space-between;
    padding: 10px;
    border-radius: 3px;

    .tag_item {
      .label {
        color: #999;
      }

      .value {
        font-weight: 500;
        margin-top: 5px;
        padding: 5px;
        display: inline-block;
        position: relative;
        cursor: pointer;
      }

      .value:hover {
        background-color: #1890ff;
        color: white;
      }
    }
  }

  .card_body {
    .title_box {
      height: 34px;
      line-height: 34px;

      i {
        cursor: pointer;
        transition: transform .5s;
        //transform: rotate(-90deg);
        font-weight: 600;
        padding: 0 8px;
      }

      .is-active {
        transform: rotate(-180deg);
      }

      margin: 10px 0 20px 0;
      background-color: #f2f2f2;
    }

    .el-tree {
      background-color: #fcfcfc;
    }

    .el-tabs__content {
      height: calc(100vh - 189px);
      overflow: auto;
      .el-tab-pane{
        height: 100%;
      }
    }


  }


  .card_footer {
    position: absolute;
    bottom: 50px;
    right: 50px;
  }

  .close {
    cursor: pointer;
    border: none;
    outline: none;
    width: 40px;
    height: 40px;
    font-size: 22px;
    line-height: 40px;
    text-align: center;
    background-color: #1890ff;
    border-radius: 5px;
    position: absolute;
    top: 50%;
    left: -35px;
    box-shadow: 5px 5px 20px #ccc;

    i {
      color: #fff;
      font-weight: 600;
    }
  }

  .close:hover {
    background-color: #f56c6c;

    i {
      transition: transform .5s;
      transform: rotate(90deg);
    }
  }


  .fixed_coperate {
    text-align: center;
    height: 80px;
    line-height: 80px;
    background-color: #f2f2f2;
    z-index: 3;
  }



}

.fade-enter-active, .fade-leave-active {
  transition: all .6s;
}

.fade-enter, .fade-leave-to {
  width: 0;
}
</style>
