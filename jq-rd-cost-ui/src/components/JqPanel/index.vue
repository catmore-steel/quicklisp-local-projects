<template>
  <div class="jq_panel">
    <div class="jq_panel_header" @click="changeIsOpen">
      <div style="margin-left:20px;font-size: 16px;">
        <span>{{title}}</span>
        <div style="float: right;margin-right: 20px">
          <i class="el-icon-arrow-down" :class="!isOpen?'is-active':''"></i>
        </div>
      </div>
    </div>
    <el-collapse-transition>
      <div class="jq_panel_content" v-show="isOpen">
        <slot></slot>
      </div>
    </el-collapse-transition>
  </div>
</template>

<script>
  export default {
    name: 'JqPanel',
    data() {
      return {
        isOpen: this.show
      }
    },
    props: {
      title: {
        type: String,
        default: '默认标题'
      },
      show: {
        type: Boolean,
        default: true
      }
    },
    watch: {
      show(newval, oldVal) {
        this.isOpen = newval
      }
    },
    methods: {
      changeIsOpen() {
        this.isOpen = !this.isOpen
      }

    }
  }
</script>

<style lang="scss">
  .jq_panel {

    border-top: 1px solid #dfe6ec;
    border-bottom: 1px solid #dfe6ec;

    .jq_panel_header {
      border-bottom-color: transparent;
      background: #eef1f6;
      height: 35px;
      line-height: 34px;

      i {
        cursor: pointer;
        transition: transform .5s;
        font-weight: 600;
        padding: 0 8px;
      }

      .is-active {
        transform: rotate(-90deg);
      }
    }

    .jq_panel_header:hover {
      background-color: #eaf2ff;
    }

    .jq_panel_content {
      padding: 10px;
    }

  }
</style>
