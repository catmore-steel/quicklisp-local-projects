<template>
  <div>
    <el-table :data="accountSplitTable">
      <el-table-column prop="feeItemName" label="费用项目" width="400"></el-table-column>
      <el-table-column prop="codesStr" label="关联会计科目" width="800"></el-table-column>
      <el-table-column label="操作" align="center" class-name="small-padding fixed">
        <template slot-scope="scope">
          <el-button
            type="text"
            icon="el-icon-edit"
            @click="accountSplitUpdate(scope.row)"
          >修改
          </el-button>
        </template>
      </el-table-column>
    </el-table>
    <accountSplit-detail :show="accountSplitCard.show" v-model="accountSplitCard.key"
                         :deptType="accountSplitCard.deptType" :feeItem="accountSplitCard.feeItem"
                         @handleClose="accountSplitClose"
    />
  </div>
</template>

<script>
import accountSplitDetail from '@/views/system/conf/common/accountSplitDetail.vue'
import { listByDeptType } from '@/api/conf/accountSplit'

export default {
  name: 'accountSplitTable',
  components: { accountSplitDetail },
  data() {
    return {
      queryParams: {},
      accountSplitTable: [],
      accountSplitCard: {
        show: false,
        key: null,
        deptType: null,
        feeItem: null
      }
    }
  },
  props: {
    value: {
      type: String
    }
  },
  watch: {
    value(data) {
      this.queryParams.deptType = data
      this.getDetail()
    }
  },
  created() {
    this.getDetail()
  },
  methods: {
    getDetail() {
      listByDeptType(this.queryParams).then(res => {
        this.accountSplitTable = res.data
      })
    },
    accountSplitUpdate(row) {
      this.accountSplitCard = {
        show: true,
        key: row.id,
        deptType: this.value,
        feeItem: row.feeItem
      }
    },
    accountSplitClose() {
      this.accountSplitCard = {
        show: false,
        key: null,
        deptType: null,
        feeItem: null
      }
      this.getDetail()
    }
  }
}
</script>

<style scoped>

</style>
