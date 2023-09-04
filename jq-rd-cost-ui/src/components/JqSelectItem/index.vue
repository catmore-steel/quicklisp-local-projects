<template>
  <el-select v-model="itemNoIn" placeholder="请输入申报项目"  :disabled="true">
    <el-option
      v-for="dict in itemInfoOptions"
      :key="dict.itemNo"
      :label="dict.itemName"
      :value="dict.itemNo"
    ></el-option>
  </el-select>
</template>

<script>

  import {projectBaseItemInfoFindList} from '@/api/project/baseItemInfo'
export default {
  name: "JqSelectItem",
  data() {
    return {
      companyIdIn: null,
      itemNoIn: null,
      itemInfoOptions:[],
    }
  },
  props: {
    companyId: {
      type: Number,
      default: null
    },
    itemNo: {
      type: String,
      default: null
    }
  },
  created() {
    // this.queryParams.itemNo = this.$store.state.item.itemNo;
    if (this.companyId  == null){
      this.companyIdIn = this.$store.state.item.companyId;
      this.itemNoIn = this.$store.state.item.itemNo;
      this.$emit('update:itemNo',    this.itemNoIn)
    }else{
      this.companyIdIn = this.companyId
      this.itemNoIn = this.itemNo
    }

    projectBaseItemInfoFindList(this.companyIdIn).then(response => {
      this.itemInfoOptions = response.data
    })
  },
  methods: {

  }

}
</script>

<style scoped>

</style>
