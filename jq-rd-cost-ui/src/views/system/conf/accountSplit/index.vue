<template>
  <div class="app-container">
    <el-tabs v-model="activeName" :tab-position="'left'">
      <el-tab-pane label="研发" name="1">
        <account-split-table v-model="activeName"/>
      </el-tab-pane>
      <el-tab-pane label="生产" name="2">
        <account-split-table v-model="activeName"/>
      </el-tab-pane>
      <el-tab-pane label="职能" name="3">
        <account-split-table v-model="activeName"/>
      </el-tab-pane>
      <el-tab-pane label="管理" name="4">
        <account-split-table v-model="activeName"/>
      </el-tab-pane>
      <el-tab-pane label="销售" name="5">
        <account-split-table v-model="activeName"/>
      </el-tab-pane>
    </el-tabs>
  </div>
</template>

<script>
import AccountSplitTable from '@/views/system/conf/common/accountSplitTable.vue'

export default {
  name: 'accountSplit',
  components: { AccountSplitTable },
  data() {
    return {
      activeName: '1',
    }
  }
}
</script>

<style scoped>

</style>
