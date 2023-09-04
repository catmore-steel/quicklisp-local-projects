<template>
  <div class="sequential_dialog">
    <jq-dialog
      :title="title"
      custom-class="sequential-dialog"
      fullscreen
      :visible.sync="open"
      @close="$emit('update:show', false)"
    >
      <el-form ref="detail" :model="detail">
        <table-transfer
          ref="tableTransferRef"
          :titles="['可选项', '已选项']"
          v-model="tempValue"
          filterable
          :filter-method="filterMethod"
          filter-placeholder="请输入内容"
          :data="baseAccountInfoList"
          :leftQuery="query"
          row-key="id"
          style="width: 100%;"
          :format="{
              noChecked: '${total}',
              hasChecked: '${checked}/${total}'
            }"
          @change="handleChange"
        >
          <template slot="left-header" slot-scope="scope">
            <div style="display: flex;flex-direction: row;height: 60px;">
              <div>非研发类账目</div>
              <div style="margin-left: 10px;width: 20px;">{{ scope.scope.checkedSummary }}</div>
              <div style="margin-left: 200px;">
                <el-input v-model="query" placeholder="请输入内容"/>
              </div>
            </div>
          </template>
          <template slot="right-header">
            <div style="height: 60px;">
              <el-radio-group v-model="detail.accountType" @change="accountTypeChange" size="mini">
                <el-radio-button v-for="(item, index) in accountTypeOptions" :key="index" :label="item.dictValue">
                  {{ item.dictLabel }}
                </el-radio-button>
              </el-radio-group>
            </div>
          </template>
          <template slot="left-table">
            <!--              <el-table-column label="id" prop="id" :sortable="false"/>-->
            <!--              <el-table-column label="序时账类型" prop="accountType" :sortable="false"/>-->
            <el-table-column label="日期" prop="accDate" :sortable="false"/>
            <el-table-column label="摘要" prop="summary" :sortable="false"/>
            <el-table-column label="会记科目" prop="accountSubject" :sortable="false"/>
            <el-table-column label="科目编码" prop="subjectCode" :sortable="false"/>
            <el-table-column label="科目名称" prop="subjectName" :sortable="false"/>
            <el-table-column label="借方金额" prop="debitAmount" :sortable="false"/>
          </template>

          <template slot="right-table">
            <!--              <el-table-column label="id" prop="id" :sortable="false"/>-->
            <!--              <el-table-column label="序时账类型" prop="accountType" :sortable="false"/>-->
            <el-table-column label="日期" prop="accDate" :sortable="false"/>
            <el-table-column label="摘要" prop="summary" :sortable="false"/>
            <el-table-column label="会记科目" prop="accountSubject" :sortable="false"/>
            <el-table-column label="科目编码" prop="subjectCode" :sortable="false"/>
            <el-table-column label="科目名称" prop="subjectName" :sortable="false"/>
            <el-table-column label="借方金额" prop="debitAmount" :sortable="false"/>
          </template>
        </table-transfer>


      </el-form>
      <div style="text-align: center">
        <el-button type="primary" @click="submitForm">确 定</el-button>
        <el-button @click="$emit('update:show', false)">取 消</el-button>
      </div>

    </jq-dialog>
  </div>
</template>

<script>
import JqTableMixin from '@/mixin/JqTable'
import TableTransfer from '@/components/JqTableShuttle/index.vue'
import { isNullOrEmpty } from '@/utils/jq'
import { getListByCompanyIdAndItemNo, updateAccountType } from '@/api/cost/accountInfo'

export default {
  name: 'ledgerClassification',
  mixins: [JqTableMixin],
  components: {
    TableTransfer
  },
  inject: ['handleQuery'],
  data() {
    return {
      open: this.show,
      //客户名称
      companyId: null,
      //申报项目
      itemNo: null,
      //序时账类型字典
      accountTypeOptions: [],
      //企业申报项目的序时账数组
      baseAccountInfoList: [],
      //绑定的序时账id数组，0：左侧；1：右侧
      tempValue: [[], []],
      // 表单参数
      detail: {
        accountType: '1'
      },
      query: ''
    }
  },
  props: {
    title: {
      type: String,
      required: false,
      default: '序时账归类'
    },
    show: {
      type: Boolean,
      default: false
    }
  },
  watch: {
    show() {
      this.open = this.show
      if (this.open) {
        this.itemNo = this.$store.state.item.itemNo
        this.companyId = this.$store.state.item.companyId
        getListByCompanyIdAndItemNo(this.companyId, this.itemNo).then(response => {
          this.baseAccountInfoList = response.data
          this.accountTypeChange()
        })
      }
    },
    '$store.state.item.itemNo'(newVal, oldVal) {
      this.$nextTick(() => {
        this.itemNo = this.$store.state.item.itemNo
        this.companyId = this.$store.state.item.companyId
        getListByCompanyIdAndItemNo(this.companyId, this.itemNo).then(response => {
          this.baseAccountInfoList = response.data
          this.accountTypeChange()
        })
      })
    }
  },
  created() {
    //费用类型字典
    this.getDicts('account_type').then(response => {
      this.accountTypeOptions = response.data
    })
  },

  methods: {
    /** 点击序时账类型 */
    accountTypeChange() {
      this.tempValue = [[], []]
      this.tempValue[0] = this.baseAccountInfoList.filter(item => !isNullOrEmpty(item.accountType)).map(item => item.id)
      this.tempValue[1] = this.baseAccountInfoList.filter(item => item.accountType === this.detail.accountType).map(item => item.id)
    },

    /** 提交按钮 */
    submitForm() {
      let baseAccountInfo = {
        ids: [],
        baseAccountIds: [],
        accountType: null
      }
      baseAccountInfo.ids = this.tempValue[0]
      baseAccountInfo.baseAccountIds = this.tempValue[1]
      baseAccountInfo.accountType = this.detail.accountType
      //console.log('baseAccountInfo', baseAccountInfo)
      updateAccountType({ baseAccountInfos: this.baseAccountInfoList }).then(response => {
        if (response.code === 200) {
          this.msgSuccess(response.msg)
          this.handleClose()
          this.handleQuery()
        }
      })
    },

    /** 关闭按钮 */
    handleClose() {
      this.$emit('handleClose')
    },

    /** 穿梭事件 */
    handleChange(value, direction, movedKeys) {
      //console.log('handleChange', value, direction, movedKeys)
      let self = this
      if (direction == 'right') {
        movedKeys.forEach(elt => {
          elt.accountType = self.detail.accountType
        })
      } else {
        movedKeys.forEach(elt => {
          elt.accountType = null
        })
      }
    },

    /** 搜索 */
    filterMethod(keywords, item) {
      //console.log('filterMethod', keywords, item)
      return item.summary.includes(keywords.toLowerCase())
        || item.accDate.includes(keywords.toLowerCase())
        || item.subjectCode.includes(keywords.toLowerCase())
        || item.subjectName.includes(keywords.toLowerCase())
    }
  }
}
</script>
