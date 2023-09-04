<template>
  <jq-card :show="cardShow" :cardKey="cardKey" :title="title" :tags="tags" @handleClose="handleClose">
    <template slot="new">
      <div style="text-align: left;">
        <div>1、标记 * 为必须输入表单。</div>
      </div>
    </template>
    <template slot="operate" v-if="cardKey">
      <!-- <jq-check-user :userIds="[accountSplitForm.dealUserId]">-->
      <el-button icon="el-icon-edit" type="success" @click="handleUpdate"
                 v-hasPermi="['system:accountSplit:edit']"
      >
        修改
      </el-button>
      <el-button type="danger" icon="el-icon-delete" @click="handleRevoke()"
                 v-hasPermi="['system:accountSplit:remove']"
      >
        撤销
      </el-button>
      <!--</jq-check-user>-->
    </template>
    <template>
      <el-tabs v-model="activeName">
        <el-tab-pane label="费用拆分配置" name="1">
          <accountSplit-update :accountSplitId="accountSplitId" :deptTypeUpdate="deptType"
                               :feeItemUpdate="feeItem" :disabled.sync="disabled" @handleClose="handleClose"
          />
        </el-tab-pane>
      </el-tabs>
    </template>
  </jq-card>
</template>

<script>
import accountSplitUpdate from './accountSplitUpdate'
import { isNullOrEmpty } from '@/utils/jq'
import { delAccountSplit, getAccountSplit } from '@/api/conf/accountSplit'

export default {
  name: 'accountSplitDetail',
  components: {
    accountSplitUpdate
  },
  provide() {
    return {
      handleComplete: this.handleComplete
    }
  },
  // inject: ['handleQuery'],
  data() {
    return {
      title: '',
      accountSplitForm: {},
      activeName: '1',
      cardShow: false,
      cardKey: null,
      tags: [],
      accountSplitId: null,
      deptTypeUpdate: null,
      feeItem: null,
      edit: false,  //开启编辑
      disabled: false
    }
  },
  props: {
    show: {
      type: Boolean,
      default: false
    },
    value: {
      type: Number
    },
    deptType: {
      type: String
    },
    feeItem: {
      type: String
    }
  },
  watch: {
    show(data) {
      this.cardShow = data
    },
    value(data) {
      this.activeName = '1'
      this.cardKey = data
      if (data) {
        this.disabled = true
      } else {
        this.disabled = false
      }
      this.getDetail()
    },
    deptType(val) {
      this.deptType = val
    },
    feeItem(val) {
      this.feeItem = val
    }
  },
  created() {
    this.getDetail()
  },
  mounted() {
  },
  methods: {
    /** 修改操作 */
    handleUpdate() {
      this.disabled = false
    },

    /** 关闭 */
    handleClose() {
      this.cardShow = false
      this.cardKey = null
      this.$emit('handleClose')
    },

    // 获取费用拆分配置详情
    getDetail() {
      let accountSplitId = this.value
      if (isNullOrEmpty(accountSplitId)) {
        //新增
        this.title = '新增费用拆分配置'
        this.accountSplitId = null
        this.tags = null
      } else {
        getAccountSplit(accountSplitId).then(res => {
          let data = res.data
          this.accountSplitForm = res.data
          this.title = '费用拆分配置详情'
          this.accountSplitId = data.id
          this.tags = [{ '创建人': data.createBy }, { '创建时间': data.createTime }, { '更新人': data.updateBy }, { '更新时间': data.updateTime }]
        })
      }
    },

    /** 撤销 */
    handleRevoke() {
      const ids = this.accountSplitId
      this.$confirm('是否确认撤销费用拆分配置编号为"' + ids + '"的数据项?', '警告', {
        confirmButtonText: '确定',
        cancelButtonText: '取消',
        type: 'warning'
      }).then(function() {
        return delAccountSplit(ids)
      }).then(() => {
        this.msgSuccess('删除成功')
        this.handleClose()
        // this.$emit('handleQuery')
      }).catch(() => {
      })
    }
  }
}
</script>
