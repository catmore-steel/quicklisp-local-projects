<template>
  <jq-card :show="cardShow" :cardKey="cardKey" :title="title" :tags="tags" @handleClose="handleClose">
    <template slot="new">
      <div style="text-align: left;">
        <div>物料信息</div>
      </div>
    </template>
    <template slot="operate" v-if="cardKey">
      <el-button icon="el-icon-edit" type="success" @click="handleUpdate"
                 v-hasPermi="['cost:materialInfo:edit']">修改
      </el-button>
      <el-button type="danger" icon="el-icon-delete"  @click="handleRevoke()"
                 v-hasPermi="['cost:materialInfo:remove']">删除
      </el-button>
    </template>
    <template>
      <el-tabs v-model="activeName">
        <el-tab-pane label="物料信息" name="1">
          <materialInfo-update :materialInfoId="materialInfoId" :disabled.sync="disabled" @handleClose="handleClose"/>
        </el-tab-pane>
      </el-tabs>
    </template>
  </jq-card>
</template>

<script>
  import materialInfoUpdate from './materialInfoUpdate'
  import {isNullOrEmpty} from '@/utils/jq'
  import {delMaterialInfo, getMaterialInfo} from '@/api/cost/materialInfo'

  export default {
    name: 'materialInfoDetail',
    components: {
      materialInfoUpdate,
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
        materialInfoForm: {},
        activeName: '1',
        cardShow: false,
        cardKey: null,
        tags: [],
        materialInfoId: null,
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

      // 获取企业物料信息详情
      getDetail() {
        let materialInfoId = this.value
        if (isNullOrEmpty(materialInfoId)) {
          //新增
          this.title = '物料详细'
          this.materialInfoId = null
          this.tags = null
        } else {
          getMaterialInfo(materialInfoId).then(res => {
            let data = res.data
            this.materialInfoForm = res.data
            this.title = '企业物料信息详情'
            this.materialInfoId = data.id
            this.tags = [{'物料编号': data.materialNo}, {'物料名称': data.materialName}, {'规格': data.specs}, {'单位': data.unit}]
          });
        }
      },

      /** 删除 */
      handleRevoke() {
        const ids = this.materialInfoId;
        this.$confirm('是否确认删除企业物料信息编号为"' + ids + '"的数据项?', "警告", {
          confirmButtonText: "确定",
          cancelButtonText: "取消",
          type: "warning"
        }).then(function () {
          return delMaterialInfo(ids);
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
