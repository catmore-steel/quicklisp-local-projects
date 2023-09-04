<template>
  <jq-card :show="cardShow" :cardKey="cardKey" :title="title" :tags="tags" @handleClose="handleClose">
    <template slot="new">
      <div style="text-align: left;">
        <div>1、标记 * 为必须输入表单。</div>
      </div>
    </template>
    <template slot="operate" v-if="cardKey">
      <!-- <jq-check-user :userIds="[patentInfoForm.dealUserId]">-->
      <el-button icon="el-icon-edit" type="success" @click="handleUpdate"
                 v-hasPermi="['cost:patentInfo:edit']">
        修改
      </el-button>
      <el-button type="danger" icon="el-icon-delete" @click="handleRevoke()"
                 v-hasPermi="['cost:patentInfo:remove']">
        删除
      </el-button>
      <!--</jq-check-user>-->
    </template>
    <template>
      <el-tabs v-model="activeName">
        <el-tab-pane label="知识产权详情" name="1">
          <patentInfo-update :patentInfoId="patentInfoId" :disabled.sync="disabled" @handleClose="handleClose"/>
        </el-tab-pane>
      </el-tabs>
    </template>
  </jq-card>
</template>

<script>
import patentInfoUpdate from './patentInfoUpdate'
import { isNullOrEmpty } from '@/utils/jq'
import { delPatentInfo, getPatentInfoByCompanyIdItemNoPatentCode } from '@/api/cost/patentInfo'

export default {
  name: 'patentInfoDetail',
  components: {
    patentInfoUpdate
  },
  provide() {
    return {
      handleComplete: this.handleComplete
    }
  },
  inject: ['handleQuery'],
  data() {
    return {
      title: '',
      patentInfoForm: {},
      activeName: '1',
      cardShow: false,
      cardKey: null,
      tags: [],
      patentInfoId: null,
      edit: false,  //开启编辑
      disabled: false,
      //知产编号
      patentCode: ''
    }
  },
  props: {
    show: {
      type: Boolean,
      default: false
    },
    value: {
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

    // 获取企业知识产权信息详情
    getDetail() {
      let patentCode = this.value
      if (isNullOrEmpty(patentCode)) {
        //新增
        this.title = '新增企业知识产权信息'
        this.patentCode = null
        this.tags = null
        this.patentInfoId = null
      } else {
        const data = {
          companyId: this.$store.state.item.companyId,
          itemNo: this.$store.state.item.itemNo,
          patentCode: patentCode
        }
        getPatentInfoByCompanyIdItemNoPatentCode(data).then(res => {
          let data = res.data
          this.patentInfoForm = res.data
          this.title = '企业知识产权信息详情'
          this.patentInfoId = data.id
          this.patentCode = data.patentCode
          this.tags = [{ '知产编号': data.patentCode }, { '知产名称': data.patentName }, { '知产类别': data.patentTypeName }, { '申请日期': data.applyDate }]
        })
      }
    },

    /** 撤销 */
    handleRevoke() {
      const ids = this.patentInfoId
      this.$confirm('是否确认撤销企业知识产权信息编号为"' + this.patentCode + '"的数据项?', '警告', {
        confirmButtonText: '确定',
        cancelButtonText: '取消',
        type: 'warning'
      }).then(function() {
        return delPatentInfo(ids)
      }).then(() => {
        this.msgSuccess('删除成功')
        this.handleClose()
        this.$emit('handleQuery')
      }).catch(() => {
      })
    }
  }
}
</script>
