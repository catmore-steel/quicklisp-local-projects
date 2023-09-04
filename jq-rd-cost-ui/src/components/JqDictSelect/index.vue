<template>
  <el-select v-model="dictValue"  :placeholder="placeholder" clearable @change="changeValue" :multiple="multiple" :style="selectStyle">
    <el-option
      v-for="dict in dictOptions"
      :key="dict.dictValue"
      :label="dict.dictLabel"
      :value="dict.dictValue"
    />
  </el-select>
</template>

<script>

export default {
  name: "JqDictSelect",
  data() {
    return {
      dictValue: null,
      dictOptions: [],
    }
  },
  props: {
    value: {
      require: true
    },
    dictType: {
      type: String,
      require: true
    },
    multiple:{
      type: Boolean,
      default: false
    },
    placeholder: {
      type: String,
      default: "请选择"
    },
    selectStyle:{
      type: String,
      default: ""
    }
  },
  watch: {
    value(val) {
      this.dictValue = val;
    }
  },
  created() {
    this.getDicts(this.dictType).then(response => {
      this.dictOptions = response.data
    })
  },
  methods: {
    changeValue(e) {
      this.$emit('update:value', this.dictValue)
      this.$emit('change',this.dictValue);
    }
  },


}
</script>

<style scoped>

</style>
