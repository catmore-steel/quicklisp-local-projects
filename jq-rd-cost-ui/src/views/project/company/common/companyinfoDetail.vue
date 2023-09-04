<template>
  <jq-card :show="cardShow" :cardKey="cardKey" :title="title" :tags="tags" @handleClose="handleClose">
    <template slot="new">
      <div style="text-align: left;">
          <div>1、标记 * 为必须输入表单。</div>
      </div>
    </template>
    <template slot="operate" v-if="cardKey">
      <!-- <jq-check-user :userIds="[companyinfoForm.dealUserId]">-->
        <el-button icon="el-icon-edit" type="success" @click="handleUpdate"
                   v-hasPermi="['project:companyinfo:edit']">
          修改
        </el-button>
        <el-button type="danger" icon="el-icon-delete" @click="handleRevoke()"
                 v-hasPermi="['project:companyinfo:remove']">
          撤销
        </el-button>
      <!--</jq-check-user>-->
    </template>
    <template>
      <el-tabs v-model="activeName">
        <el-tab-pane label="企业信息" name="1">
            <companyinfo-update :companyinfoId="companyinfoId" :disabled.sync="disabled" @handleClose="handleClose"/>
        </el-tab-pane>
      </el-tabs>
    </template>
  </jq-card>
</template>

<script>
  import companyinfoUpdate from './companyinfoUpdate'
  import {isNullOrEmpty} from '@/utils/jq'
  import {delCompanyinfo, getCompanyinfo} from '@/api/project/companyinfo'
  import { selectItemByCompanyId } from '@/api/project/baseItemInfo'

  export default {
    name: 'companyinfoDetail',
    components: {
      companyinfoUpdate,
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
        companyinfoForm: {},
        activeName: '1',
        cardShow: false,
        cardKey: null,
        tags: [],
        companyinfoId: null,
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
      row:{
        type:Object
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

      // 获取企业信息详情
      getDetail() {
        let companyinfoId = this.value
        if (isNullOrEmpty(companyinfoId)) {
          //新增
          this.title = '新增企业信息'
          this.companyinfoId = null
          this.tags = null
        } else {
          getCompanyinfo(companyinfoId).then(res => {
            let data = res.data
            this.companyinfoForm = res.data
            this.title = '企业信息详情'
            this.companyinfoId = data.id
            console.log("*******",this.row)
            this.tags = [{'企业名称': this.row.companyName}, {'统一信用代码': this.row.creditCode}, {'成立时间': this.row.foundDate}, {'行业类别': this.row.title}]
          });
        }
      },

      /** 撤销 */
      handleRevoke() {
        selectItemByCompanyId(this.companyinfoId).then(res => {
          if (res.data && res.data.length > 0) {
            this.msgError('该企业下存在项目，不可删除！')
          } else {
            const ids = this.companyinfoId;
            this.$confirm('是否确认撤销企业信息编号为"' + ids + '"的数据项?', "警告", {
              confirmButtonText: "确定",
              cancelButtonText: "取消",
              type: "warning"
            }).then(function () {
              return delCompanyinfo(ids);
            }).then(() => {
              this.msgSuccess("删除成功");
              this.handleClose()
              this.$emit('handleQuery')
            }).catch(() => {
            })
          }
        })

      },
    }
  }
</script>
