export default {
  data(){
    return {
    }
  },
  created() {
  },
  methods:{
    getJqTableData() {
        this.$refs.JqTableRef.getList()
    },
    // 删除选中行的时候 表格选中的数据还在
    resetSelectedRows(){
      this.$refs.JqTableRef.key++
      this.$refs.JqTableRef.checkRows= [],
      this.$refs.JqTableRef.checkLength=0,
      this.$refs.JqTableRef.ids =[]
    },
    // 获取当前页和size(为了导出当前页)
    getPageSize() {
      return {
        pageNum: this.$refs.JqTableRef.pageNum,
        pageSize: this.$refs.JqTableRef.pageSize,
        orders: this.$refs.JqTableRef.orders
      }
    }
  }
}
