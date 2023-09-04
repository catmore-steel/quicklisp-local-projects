<template>
<!--  :disabled="true"-->
  <el-select v-model="companyIdIn" placeholder="请选择客户名称"  :disabled="true">
    <el-option
      v-for="dict in companyOptions"
      :key="dict.id"
      :label="dict.companyName"
      :value="dict.id"
    ></el-option>
  </el-select>
</template>

<script>
  import {getCompanyList} from '@/api/project/companyinfo'

export default {
  name: "JqSelectCompany",
  data() {
    return {
      companyIdIn: null,
      companyOptions:[],
    }
  },
  props: {
    companyId: {
      type: Number,
      default: null
    }
  },
  created() {
    // this.queryParams.itemNo = this.$store.state.item.itemNo;
    if (this.companyId  == null){
      this.companyIdIn = this.$store.state.item.companyId;
      this.$emit('update:companyId',  this.companyIdIn)
    }else{
      this.companyIdIn = this.companyId
    }
    getCompanyList().then(response => {
      this.companyOptions = response.data



    })
  },
  methods: {

  }


}
</script>

<style scoped>

</style>
