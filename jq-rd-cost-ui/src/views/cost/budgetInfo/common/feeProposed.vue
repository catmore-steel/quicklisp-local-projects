<template>
  <div class="fee_proposed_dialog">
    <jq-dialog
        :title="title"
        custom-class="fee-proposed-dialog"
        fullscreen
        :visible.sync="open"
        @close="$emit('update:show', false)"
    >

      <el-form ref="detail" :model="detail" :rules="rules" label-width="130px">
        <el-row>
          <el-col :span="8">
            <el-form-item label="一年营业收入" prop="yearIncome" label-width="200px">
              <el-input v-model="detail.yearIncome" placeholder="一年营业收入"
                        @change="calculateTotal"
              />
            </el-form-item>
          </el-col>
          <el-col :span="8">
            <el-form-item label="研发费用拟定占比%" prop="devRate" label-width="200px">
              <el-input v-model.number="detail.devRate" placeholder="研发费用拟定占比%"
                        @input="val => detail.devRate = val.replace(/^([0-9]\d*\.?\d{0,2})?.*$/,'$1')"
                        @change="calculateTotal"
              />
            </el-form-item>
          </el-col>
          <el-col :span="8">
            <el-form-item label="研发费用拟定总额" prop="devSum" label-width="200px">
              <el-input v-model.number="detail.devSum = detail.yearIncome * detail.devRate / 100" disabled/>
            </el-form-item>
          </el-col>
        </el-row>
        <el-table :data="budgetAccountInfoList">
          <el-table-column prop="name" label="费用类型" :sortable="false"/>
          <el-table-column label="人员人工费用（%）" :sortable="false">
            <template slot-scope="scope">
              <el-input :disabled="scope.row.name === '拟定投入总额' || scope.row.name === '序时账分类总额'"
                        @input="val => detail[scope.row.userKey] = val.replace(/^([0-9]\d*\.?\d{0,2})?.*$/,'$1')"
                        @change="calculateTotal"
                        v-model.number="detail[scope.row.userKey]"
              />
            </template>
          </el-table-column>
          <el-table-column label="直接投入费用（%）" :sortable="false">
            <template slot-scope="scope">
              <el-input :disabled="scope.row.name === '拟定投入总额' || scope.row.name === '序时账分类总额'"
                        @input="val => detail[scope.row.materialKey] = val.replace(/^([0-9]\d*\.?\d{0,2})?.*$/,'$1')"
                        @change="calculateTotal"
                        v-model.number="detail[scope.row.materialKey]"
              />
            </template>
          </el-table-column>
          <el-table-column label="折旧费用（%）" :sortable="false">
            <template slot-scope="scope">
              <el-input :disabled="scope.row.name === '拟定投入总额' || scope.row.name === '序时账分类总额'"
                        @input="val => detail[scope.row.equipmentKey] = val.replace(/^([0-9]\d*\.?\d{0,2})?.*$/,'$1')"
                        @change="calculateTotal"
                        v-model.number="detail[scope.row.equipmentKey]"
              />
            </template>
          </el-table-column>
          <el-table-column label="无形资产摊销（%）" :sortable="false">
            <template slot-scope="scope">
              <el-input :disabled="scope.row.name === '拟定投入总额' || scope.row.name === '序时账分类总额'"
                        @input="val => detail[scope.row.invisibleKey] = val.replace(/^([0-9]\d*\.?\d{0,2})?.*$/,'$1')"
                        @change="calculateTotal"
                        v-model.number="detail[scope.row.invisibleKey]"
              />
            </template>
          </el-table-column>
          <el-table-column label="新产品设计费等（%）" :sortable="false">
            <template slot-scope="scope">
              <el-input :disabled="scope.row.name === '拟定投入总额' || scope.row.name === '序时账分类总额'"
                        @input="val => detail[scope.row.designKey] = val.replace(/^([0-9]\d*\.?\d{0,2})?.*$/,'$1')"
                        @change="calculateTotal"
                        v-model.number="detail[scope.row.designKey]"
              />
            </template>
          </el-table-column>
          <el-table-column label="其他相关费用（%）" :sortable="false">
            <template slot-scope="scope">
              <el-input :disabled="scope.row.name === '拟定投入总额' || scope.row.name === '序时账分类总额'"
                        @input="val => detail[scope.row.otherKey] = val.replace(/^([0-9]\d*\.?\d{0,2})?.*$/,'$1')"
                        @change="calculateTotal"
                        v-model.number="detail[scope.row.otherKey]"
              />
            </template>
          </el-table-column>
        </el-table>
      </el-form>
      <div slot="footer" class="dialog-footer">
        <el-button type="primary" @click="submitForm">确 定</el-button>
        <el-button @click="handleClose">取 消</el-button>
      </div>

    </jq-dialog>
  </div>
</template>

<script>
import { isNullOrEmpty } from '@/utils/jq'
import { addGeneralBudget, getFeeProposedDetail } from '@/api/cost/generalbudget'

export default {
  name: 'feeProposed',
  components: {},
  inject: ['handleQuery'],
  data() {
    return {
      open: this.show,
      //客户名称
      companyId: null,
      //申报项目
      itemNo: null,
      //预算研发项目拟定
      budgetAccountInfoList: [
        { name: '拟定占比', userKey: 'userFee', materialKey: 'materialFee', equipmentKey: 'equipmentFee', invisibleKey: 'invisibleFee', designKey: 'designFee', otherKey: 'otherFee' },
        { name: '拟定投入总额', userKey: 'totalUserFee', materialKey: 'totalMaterialFee', equipmentKey: 'totalEquipmentFee', invisibleKey: 'totalInvisibleFee', designKey: 'totalDesignFee', otherKey: 'totalOtherFee' },
        { name: '序时账分类总额', userKey: 'xsUserFee', materialKey: 'xsMaterialFee', equipmentKey: 'xsEquipmentFee', invisibleKey: 'xsInvisibleFee', designKey: 'xsDesignFee', otherKey: 'xsOtherFee' }
      ],

      //预算研发项目拟定信息
      detail: {},
      //表单校验
      rules: {
        devRate: [
          { type: 'number', min: 0, max: 100, message: '研发费用拟定占比在 0 到 100 之间' }
        ]
      }
    }
  },
  props: {
    title: {
      type: String,
      required: false,
      default: '研发费用拟定'
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
        this.getDetail()
      }
    },
    '$store.state.item.itemNo'(newVal, oldVal) {
      this.$nextTick(() => {
        this.itemNo = this.$store.state.item.itemNo
        this.companyId = this.$store.state.item.companyId
        this.getDetail()
      })
    }
  },
  created() {
  },
  methods: {
    /** 表单重置 */
    reset() {
      this.detail = {
        yearIncome: '',
        devRate: 0,
        devSum: 0,
        userFee: 0,
        materialFee: 0,
        equipmentFee: 0,
        invisibleFee: 0,
        designFee: 0,
        otherFee: 0,
        itemNo: null,
        companyId: null,
        tenantId: null,
        revision: null,
        createBy: null,
        createTime: null,
        updateBy: null,
        updateTime: null,
        remark: null,
        delFlag: null
      }
      this.resetForm('detail')
    },

    /** 查询预算研发项目拟定信息 */
    getDetail() {
      this.reset()
      getFeeProposedDetail(this.companyId, this.itemNo).then(res => {
        this.detail = res.data
        this.calculateTotal()
      })
    },

    /** 计算拟定投入总额 */
    calculateTotal() {
      //空值则为零
      this.detail.yearIncome = isNullOrEmpty(this.detail.yearIncome) ? 0 : this.detail.yearIncome
      this.detail.devRate = isNullOrEmpty(this.detail.devRate) ? 0 : Number(this.detail.devRate)
      this.detail.userFee = isNullOrEmpty(this.detail.userFee) ? 0 : Number(this.detail.userFee)
      this.detail.materialFee = isNullOrEmpty(this.detail.materialFee) ? 0 : Number(this.detail.materialFee)
      this.detail.equipmentFee = isNullOrEmpty(this.detail.equipmentFee) ? 0 : Number(this.detail.equipmentFee)
      this.detail.invisibleFee = isNullOrEmpty(this.detail.invisibleFee) ? 0 : Number(this.detail.invisibleFee)
      this.detail.designFee = isNullOrEmpty(this.detail.designFee) ? 0 : Number(this.detail.designFee)
      this.detail.otherFee = isNullOrEmpty(this.detail.otherFee) ? 0 : Number(this.detail.otherFee)
      //计算拟定投入总额
      this.detail.totalUserFee = Number(this.detail.devSum) * this.detail.userFee / 100
      this.detail.totalMaterialFee = Number(this.detail.devSum) * this.detail.materialFee / 100
      this.detail.totalEquipmentFee = Number(this.detail.devSum) * this.detail.equipmentFee / 100
      this.detail.totalInvisibleFee = Number(this.detail.devSum) * this.detail.invisibleFee / 100
      this.detail.totalDesignFee = Number(this.detail.devSum) * this.detail.designFee / 100
      this.detail.totalOtherFee = Number(this.detail.devSum) * this.detail.otherFee / 100
    },

    /** 提交按钮 */
    submitForm() {
      this.$refs['detail'].validate(valid => {
        if (valid) {
          if (this.checkData()) {
            return
          }

          addGeneralBudget(this.detail).then(response => {
            if (response.code === 200) {
              this.msgSuccess(response.msg)
              this.handleClose()
              this.handleQuery()
            }
          })
        }
      })
    },

    /** 保存时数据校验 */
    checkData() {
      let userProposedRatio = isNullOrEmpty(this.detail.userFee) ? 0 : this.detail.userFee
      let materialProposedRatio = isNullOrEmpty(this.detail.materialFee) ? 0 : this.detail.materialFee
      let equipmentProposedRatio = isNullOrEmpty(this.detail.equipmentFee) ? 0 : this.detail.equipmentFee
      let invisibleProposedRatio = isNullOrEmpty(this.detail.invisibleFee) ? 0 : this.detail.invisibleFee
      let designProposedRatio = isNullOrEmpty(this.detail.designFee) ? 0 : this.detail.designFee
      let otherProposedRatio = isNullOrEmpty(this.detail.otherFee) ? 0 : this.detail.otherFee
      let proposedRatio = Number(userProposedRatio) + Number(materialProposedRatio) + Number(equipmentProposedRatio)
          + Number(invisibleProposedRatio) + Number(designProposedRatio) + Number(otherProposedRatio)

      if (proposedRatio !== 100) {
        this.msgError('【拟定占比】总和必须等于100！')
        return true
      }
    },

    /** 关闭弹窗 */
    handleClose() {
      this.$emit('handleClose')
    }

  }
}
</script>
