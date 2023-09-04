<template>
  <div class="export_dialog">
    <jq-dialog
      :title="title"
      custom-class="export-dialog"
      width="500px"
      :visible.sync="open"
      @close="$emit('update:show', false)"
      append-to-body>
      <el-radio-group v-model="exportType" @change="$emit('update:type', exportType)">
        <el-radio :label="1">导出所有查询数据</el-radio>
        <el-radio :label="2">导出当前页数据</el-radio>
        <el-radio :label="3">导出选中数据</el-radio>
      </el-radio-group>
      <div slot="footer">
        <el-button type="warning" @click="handleExport">导 出</el-button>
        <el-button @click="$emit('update:show', false)">取 消</el-button>
      </div>
    </jq-dialog>
  </div>

</template>

<script>
  export default {
    name: 'ExportDialog',
    data() {
      return {
        exportType: this.type,
        open: this.show
      }
    },
    props: {
      title: {
        type: String,
        required: false,
        default: '导出类型'
      },
      show: {
        type: Boolean,
        default: false
      },
      type: {
        type: Number,
        default: 1
      }
    },
    watch: {
      show() {
        this.open = this.show
      },
      type() {
        this.exportType = this.type
      }
    },
    methods: {
      handleExport() {
        this.$emit('handleExport')
      }
    }
  }
</script>

<style scoped  lang="scss">

</style>
