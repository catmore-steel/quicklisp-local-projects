<template>
  <div class="fs-transfer">
    <transfer-panel
      v-bind="$props"
      ref="leftPanel"
      :data="sourceData"
      :title="titles[0] || ''"
      :default-checked="leftDefaultChecked"
      :query="leftQuery"
      @checked-change="onSourceCheckedChange"
    >
      <slot v-if="$slots.default" slot="table" />
      <slot v-else name="left-table" slot="table" />
      <slot name="left-footer" slot="footer"></slot>
      <slot name="left-header" slot="header" slot-scope="scope" v-bind="scope"></slot>
      
  
    </transfer-panel>
    <div class="fs-transfer-option">
      <div>
      <el-button
        type="primary"
        :disabled="rightChecked.length === 0"
        @click.native="addToLeft"
      >
        <i class="el-icon-arrow-left"></i>
        <span v-if="buttonTexts[0] !== undefined">{{ buttonTexts[0] }}</span>
      </el-button>
      </div>
      <div style="margin: 5px auto;">
      <el-button
        type="primary"
        :disabled="leftChecked.length === 0"
        @click.native="addToRight"
      >
        <span v-if="buttonTexts[1] !== undefined">{{ buttonTexts[1] }}</span>
        <i class="el-icon-arrow-right"></i>
      </el-button>
      </div>
    </div>
    <transfer-panel
      v-bind="$props"
      ref="rightPanel"
      :data="targetData"
      :title="titles[1] || ''"
      :default-checked="rightDefaultChecked"
      :query="rightQuery"
      @checked-change="onTargetCheckedChange"
    >
      <slot v-if="$slots.default" slot="table" />
      <slot v-else name="right-table" slot="table" />
      <slot name="right-footer" slot="footer"></slot>
      <slot name="right-header" slot="header"></slot>
    </transfer-panel>
  </div>
</template>

<script>
  import TransferPanel from '@/components/JqTableShuttle/TableTransfer.vue'

  export default {
    name: 'TableTransfer',
    components: {
      TransferPanel,
    },
    props: {
      data: {
        type: Array,
        default() {
          return []
        }
      },
      rowKey: {
        type: String,
        required: true
      },
      value: {
        type: Array,
        default() {
          return [[], []]
        }
      },
      buttonTexts: {
        type: Array,
        default() {
          return []
        }
      },
      leftDefaultChecked: {
        type: Array,
        default() {
          return []
        }
      },
      beforeRightButtonClick: Function,
      beforeLeftButtonClick: Function,
      rightDefaultChecked: {
        type: Array,
        default() {
          return []
        }
      },
      filterable: {
        type: Boolean,
        default: false
      },
      filterMethod: Function,
      selectable: Function,
      panelStyle: {
        type: Object,
        default() {
          return {}
        }
      },
      targetOrder: {
        type: String,
        default: 'original'
      },
      titles: {
        type: Array,
        default() {
          return []
        }
      },
      format: {
        type: Object,
        default() {
          return {}
        }
      },
      tableProps: Object,
      leftQuery:String,
      rightQuery:{
        type: String,
        default() {
          return ''
        }
      }
    },
    data() {
      return {
        leftChecked: [],
        rightChecked: []
      }
    },
    computed: {
      dataObj() {
        const key = this.rowKey
        return this.data.reduce((o, cur) => (o[cur[key]] = cur) && o, {})
      },
      sourceData() {
        return this.data.filter(i => !this.value[0].includes(i[this.rowKey]))
      },
      targetData() {
        if (this.targetOrder === 'original') {
          return this.data.filter(i => this.value[1].includes(i[this.rowKey]))
        } else {
          return this.value[1].reduce((arr, cur) => {
            const val = this.dataObj[cur]
            if (val) {
              arr.push(val)
            }
            return arr
          }, [])
        }
      }
    },
    methods: {
      addToLeft() {
        if (this.beforeLeftButtonClick) {
          if (!this.beforeLeftButtonClick()) return
        }
        let currentValue = this.value[0].slice()
        let value_1 = this.value[1];
        const key = this.rowKey;
        let delete_offset = 0;
        this.rightChecked.forEach((item, index) => {
          const findIndex = currentValue.indexOf(item[key])
          if (findIndex > -1) {
            value_1.splice(index - delete_offset, 1);
            delete_offset++;
            currentValue.splice(findIndex, 1)
          }else {
          }
        });
        let value = this.value;
        value[0] = currentValue;
        value[1] = value_1;
        value = value.concat([]);
        console.log('value', value);
        this.$emit('input', value)
        this.$emit('change', currentValue, 'left', this.rightChecked)
      },
      addToRight() {
        if (this.beforeRightButtonClick) {
          if (!this.beforeRightButtonClick()) return
        }
        let value_0 = this.value[0];
        let currentValue = this.value[1].slice()
        const itemsToBeMoved = []
        const key = this.rowKey
        this.data.forEach(item => {
          const itemKey = item[key]  //[1,3,40]
          let findIndex = this.leftChecked.findIndex(i => i[key] === itemKey);
          if (
            findIndex > -1 &&
            this.value[1].indexOf(itemKey) === -1
          ) {
            value_0.push(itemKey);
            itemsToBeMoved.push(itemKey)
          }
        })
        // alert(itemsToBeMoved) = 40
        currentValue = this.targetOrder === 'unshift'
          ? itemsToBeMoved.concat(currentValue)
          : currentValue.concat(itemsToBeMoved)
        // alert(currentValue)  =1,3,,40
        let value = this.value;
        value[0] = value_0;
        value[1] = currentValue;
        value = value.concat([]);
        this.$emit('input', value);
        this.$emit('change', value, 'right', this.leftChecked)
      },
      onSourceCheckedChange(val, movedKeys, checkedRows) {
        this.leftChecked = checkedRows || []
        if (movedKeys === undefined) return
        this.$emit('left-check-change', val, movedKeys, checkedRows)
      },
      onTargetCheckedChange(val, movedKeys, checkedRows) {
        this.rightChecked = checkedRows || []
        if (movedKeys === undefined) return
        this.$emit('right-check-change', val, movedKeys, checkedRows)
      },
      clearQuery(direction) {
        if (direction === 'left') {
          this.$refs.leftPanel.clearQuery()
        } else if (direction === 'right') {
          this.$refs.rightPanel.clearQuery()
        }
      }
    }
  }
</script>

<style lang="scss" scoped>
  .fs-transfer {
    text-align: left;
    height: 450px;
    display: flex;
    align-items: center;
    width: 850px;

    .fs-transfer-option {
      display: inline-block;
      vertical-align: middle;
      margin: 0 5px;
    }
  }
</style>
