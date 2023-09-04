<template>
  <jq-card :show="cardShow" :cardKey="cardKey" :title="title" :tags="tags" @handleClose="handleClose">
    <template slot="new">
      <div style="text-align: left;">
          <div>1、标记 * 为必须输入表单。</div>
      </div>
    </template>
    <template slot="operate" v-if="cardKey">
      <!-- <jq-check-user :userIds="[deptInfoForm.dealUserId]">-->
        <el-button icon="el-icon-edit" type="success" @click="handleUpdate"
                   v-hasPermi="['cost:deptInfo:edit']">
          修改
        </el-button>
        <el-button type="danger" icon="el-icon-delete" @click="handleRevoke()"
                 v-hasPermi="['cost:deptInfo:remove']">
          删除
        </el-button>
      <!--</jq-check-user>-->
    </template>
    <template>
      <el-tabs v-model="activeName">
        <el-tab-pane label="企业部门信息" name="1">
            <deptInfo-update :deptInfoId="value" :deptInfoPid="deptInfoPid" :disabled.sync="disabled" @handleClose="handleClose"/>
        </el-tab-pane>

      </el-tabs>
    </template>
  </jq-card>
</template>

<script>
  import deptInfoUpdate from './deptInfoUpdate'
  import {isNullOrEmpty} from '@/utils/jq'
  import {delDeptInfo, getDeptInfo} from '@/api/cost/deptInfo'

  export default {
    name: 'deptInfoDetail',
    components: {
      deptInfoUpdate,
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
        deptInfoForm: {},
        activeName: '1',
        cardShow: false,
        cardKey: null,
        tags: [],
        deptInfoId: null,
        deptInfoPid: null,
        edit: false,  //开启编辑
        disabled: false,
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
      currentNodeData:{
        type: Object
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
      currentNodeData(data) {
        console.log('currentNodeData------',data,data.id)
        this.deptInfoPid = data.id
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

      // 获取企业部门信息详情
      getDetail() {
        let deptInfoId = this.value
        if (isNullOrEmpty(deptInfoId)) {
          //新增
          this.title = '部门信息'
          this.deptInfoId = null
          this.tags = null
        } else {
          getDeptInfo(deptInfoId).then(res => {
            let data = res.data
            this.deptInfoForm = res.data
            this.title = '部门详情'
            this.deptInfoId = data.id
            this.tags = [{'创建人': data.createBy}, {'创建时间': data.createTime}, {'更新人': data.updateBy}, {'更新时间': data.updateTime}]
          });
        }
      },

      /** 撤销 */
      handleRevoke() {
        const ids = this.deptInfoId;
        this.$confirm('是否确认撤销企业部门信息编号为"' + ids + '"的数据项?', "警告", {
          confirmButtonText: "确定",
          cancelButtonText: "取消",
          type: "warning"
        }).then(function () {
          return delDeptInfo(ids);
        }).then(() => {
          this.msgSuccess("删除成功");
          this.handleClose()
          this.handleQuery()
        }).catch(() => {
        })
      },
    }
  }
</script>
