import type {VisibleItems} from "tim/plugin/dataview/util";
import {clamp} from "tim/plugin/dataview/util";

/**
 * A single axis manager for rows or columns. Handles caching visible items, item positions and order.
 * Provides simple API for determining the visible items.
 */
export class GridAxisManager {
    /**
     * An offset list that represents start positions of each visible item, taking into account possible border size
     */
    positionStart: number[] = [];

    /**
     * List of all visible items in their correct final order. Use this to map visible row index to actual row index.
     */
    visibleItems: number[] = [];

    /**
     * List of all items in their display order.
     */
    itemOrder: number[] = [];

    /**
     * A record that maps item index to its ordinal in the visible table.
     * Essentially a reverse mapping of visibleItems.
     */
    indexToOrdinal: Record<number, number> = {};

    private totalSizeIncompleteInternal = false;

    constructor(
        private size: number,
        private isDataViewVirtual: boolean,
        private borderSpacing: number,
        private getSize: (i: number) => [number, boolean],
        private showItem: (index: number) => boolean,
        private skipItem?: (ordinal: number) => boolean
    ) {
        this.itemOrder = Array.from(new Array(size)).map((e, i) => i);
        this.refresh();
    }

    /**
     * Number of all items (visible and hidden).
     */
    get allCount() {
        return this.size;
    }

    /**
     * Total size of the axis. In other words, the sum of all grid item sizes.
     */
    get totalSize(): number {
        return this.positionStart[this.positionStart.length - 1];
    }

    /**
     * Whether the scrolling for the axis is virtual.
     * Non-virtually scrolling axis doesn't have positionStart precomputed (as they can be determined from DOM)
     * and getVisibleItemsInViewport returns all items in the axis.
     */
    get isVirtual(): boolean {
        // If the total size of the axis is precomputed, we know the axis needs virtual scrolling
        return !!this.totalSize;
    }

    get visibleCount(): number {
        return this.visibleItems.length;
    }

    /**
     * Whether the total size could not be completely determined.
     */
    get totalSizeIncomplete() {
        return this.totalSizeIncompleteInternal && this.isVirtual;
    }

    /**
     * Updates the visible items and recomputes the positions if needed.
     */
    refresh(): void {
        let currentVisible = -1;
        this.visibleItems = this.itemOrder.filter((index) => {
            const result = this.showItem(index);
            if (!result) {
                return false;
            }
            currentVisible++;
            return this.skipItem ? !this.skipItem(currentVisible) : true;
        });
        this.indexToOrdinal = {};
        for (const [ord, index] of this.visibleItems.entries()) {
            this.indexToOrdinal[index] = ord;
        }
        if (!this.isDataViewVirtual) {
            return;
        }
        this.positionStart = [0];
        let sizeOk = true;
        for (let i = 0; i <= this.visibleItems.length - 1; i++) {
            const index = this.visibleItems[i];
            const sizeRes = this.getSize(index);
            let size = sizeRes[0];
            const ok = sizeRes[1];
            if (!ok) {
                sizeOk = false;
            }
            if (size && ok) {
                size += this.borderSpacing;
            }
            this.positionStart[i + 1] = this.positionStart[i] + size;
        }
        this.totalSizeIncompleteInternal = !sizeOk;
    }

    /**
     * Gets all items that are visible in the given area.
     * Allows to specify the viewport and visible areas separately to account for overflow.
     *
     * @param vpStartPosition Start position of the viewport, in pixels.
     * @param vpSize Viewport's total size, in pixels.
     * @param visibleStartPosition Start position of the visible area of the viewport, in pixels.
     * @param visibleSize Size of the visible area of the viewport, in pixels.
     * @return A VisibleItems object that contains information about visible item indices.
     */
    getVisibleItemsInViewport(
        vpStartPosition: number,
        vpSize: number,
        visibleStartPosition: number,
        visibleSize: number
    ): VisibleItems {
        if (!this.isVirtual) {
            return {
                startPosition: 0,
                count: this.visibleItems.length,
                startOrdinal: 0,
                visibleCount: 0,
                visibleStartOrdinal: 0,
                size: 0,
            };
        }
        vpStartPosition = clamp(vpStartPosition || 0, 0, this.totalSize);
        visibleStartPosition = clamp(visibleStartPosition, 0, this.totalSize);
        vpSize = Math.min(vpSize, this.totalSize - vpStartPosition);
        visibleSize = Math.min(
            visibleSize,
            this.totalSize - visibleStartPosition
        );
        const startIndex = this.search(vpStartPosition);
        const viewStartIndex = this.search(visibleStartPosition);
        const endIndex = this.search(vpStartPosition + vpSize);
        const viewEndIndex = this.search(visibleStartPosition + visibleSize);
        const count = Math.min(
            endIndex - startIndex + 1,
            this.visibleItems.length - startIndex
        );
        const startPosition = this.positionStart[startIndex];
        const endPosition = this.positionStart[startIndex + count];
        return {
            startOrdinal: startIndex,
            count: count,
            startPosition: startPosition,
            visibleStartOrdinal: viewStartIndex,
            visibleCount: Math.min(
                viewEndIndex - viewStartIndex + 1,
                this.visibleItems.length - viewStartIndex
            ),
            size: endPosition - startPosition,
        };
    }

    private search(position: number): number {
        let start = 0;
        let end = this.positionStart.length - 1;
        while (start < end) {
            const mid = Math.floor((start + end) / 2);
            const posStart = this.positionStart[mid];
            if (position < posStart) {
                end = mid - 1;
            } else if (position > posStart) {
                start = mid + 1;
            } else {
                return mid;
            }
        }
        return end;
    }
}
