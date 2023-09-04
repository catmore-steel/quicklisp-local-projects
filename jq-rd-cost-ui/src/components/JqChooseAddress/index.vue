<!-- 级联选择器选择地址 -->
<template>
  <el-cascader
    v-model="addressArr"
    :props="{ checkStrictly: true }"
    clearable
    @change="change"
    :options="options"
    class="form-control"
  >
  </el-cascader>
</template>

<script>
import {regionData} from 'element-china-area-data'

export default {
  name: "chooseAddress",
  props: {
    spacer: {
      type: String,
      default: ','
    },
    addressStr: {
      type: String
    }
  },
  data() {
    return {
      addressArr: [],
      options: regionData,
    }
  },
  watch:{
    addressStr(val){
      if(!val) this.addressArr = []
    }
  },
  methods: {
    change(val) {
      this.addressStr = val.join(this.spacer)
      this.$emit('update:addressStr', this.addressStr)
    }
  }
}
</script>

<style scoped>

</style>
