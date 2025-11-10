'use client';

import React, { useEffect, useState, useCallback } from 'react';
import { useRouter } from 'next/navigation';
import { cardService } from '@/services/cardService';
import { CardListItem, CardListResponse } from '@/types/card';
import { Button, Input } from '@/components/ui';

/**
 * Credit Card List Screen (CCRDLIA)
 * 
 * Purpose: Display paginated list of credit cards with filtering and selection capabilities
 * 
 * Features:
 * - Paginated display (7 records per page as per COBOL spec)
 * - Filter by Account ID (11 digits) and Card Number (16 digits)
 * - Selection actions: 'S' for view details, 'U' for update
 * - Navigation: PF3 (Exit), PF7 (Previous), PF8 (Next)
 * - Full validation and error handling
 */
export default function CardsPage() {
  const router = useRouter();
  
  // State management
  const [cards, setCards] = useState<CardListItem[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [pageInfo, setPageInfo] = useState({
    currentPage: 0,
    totalPages: 0,
    totalElements: 0,
    isFirst: true,
    isLast: true,
  });
  
  // Filter state
  const [accountIdFilter, setAccountIdFilter] = useState('');
  const [cardNumberFilter, setCardNumberFilter] = useState('');
  
  // Selection state
  const [selections, setSelections] = useState<Record<string, string>>({});
  
  // Header state
  const [currentDateTime, setCurrentDateTime] = useState({
    date: '',
    time: '',
  });

  // Update date/time on mount and every second
  useEffect(() => {
    const updateDateTime = () => {
      const now = new Date();
      setCurrentDateTime({
        date: now.toLocaleDateString('en-US', {
          year: 'numeric',
          month: '2-digit',
          day: '2-digit',
        }),
        time: now.toLocaleTimeString('en-US', {
          hour: '2-digit',
          minute: '2-digit',
          second: '2-digit',
          hour12: false,
        }),
      });
    };
    
    updateDateTime();
    const interval = setInterval(updateDateTime, 1000);
    
    return () => clearInterval(interval);
  }, []);

  // Fetch cards on mount and when filters/page change
  useEffect(() => {
    fetchCards();
  }, []);

  /**
   * Fetch cards with current filters and page
   */
  const fetchCards = async (page: number = 0) => {
    try {
      setLoading(true);
      setError(null);
      
      const filters: any = {
        page,
        size: 7, // COBOL screen shows 7 records per page
      };
      
      if (accountIdFilter) {
        filters.accountId = accountIdFilter;
      }
      if (cardNumberFilter) {
        filters.cardNumber = cardNumberFilter;
      }
      
      const response: CardListResponse = await cardService.getCards(filters);
      
      setCards(response.content);
      setPageInfo({
        currentPage: response.number,
        totalPages: response.totalPages,
        totalElements: response.totalElements,
        isFirst: response.first,
        isLast: response.last,
      });
      
      // Clear selections when data changes
      setSelections({});
      
      // Display message if no records found
      if (response.content.length === 0) {
        setError('NO RECORDS FOUND FOR THIS SEARCH CONDITION');
      }
    } catch (err) {
      setError('Failed to load cards');
      console.error(err);
    } finally {
      setLoading(false);
    }
  };

  /**
   * Validate account ID filter (must be 11 digits)
   */
  const validateAccountId = (value: string): boolean => {
    if (!value) return true; // Optional field
    if (!/^\d{11}$/.test(value)) {
      setError('ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER');
      return false;
    }
    return true;
  };

  /**
   * Validate card number filter (must be 16 digits)
   */
  const validateCardNumber = (value: string): boolean => {
    if (!value) return true; // Optional field
    if (!/^\d{16}$/.test(value)) {
      setError('CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMBER');
      return false;
    }
    return true;
  };

  /**
   * Handle filter application
   */
  const handleApplyFilters = () => {
    setError(null);
    
    // Validate filters
    if (!validateAccountId(accountIdFilter)) return;
    if (!validateCardNumber(cardNumberFilter)) return;
    
    // Fetch with filters from page 0
    fetchCards(0);
  };

  /**
   * Handle selection code change
   */
  const handleSelectionChange = (cardNumber: string, value: string) => {
    const upperValue = value.toUpperCase();
    
    // Only allow S, U, or empty
    if (upperValue && upperValue !== 'S' && upperValue !== 'U') {
      setError('INVALID ACTION CODE');
      return;
    }
    
    setSelections((prev) => ({
      ...prev,
      [cardNumber]: upperValue,
    }));
    setError(null);
  };

  /**
   * Handle action execution (Enter key or button)
   */
  const handleExecuteAction = () => {
    setError(null);
    
    // Get all selected cards
    const selectedCards = Object.entries(selections).filter(
      ([_, action]) => action
    );
    
    if (selectedCards.length === 0) {
      // No selection - just refresh
      fetchCards(pageInfo.currentPage);
      return;
    }
    
    if (selectedCards.length > 1) {
      setError('MORE THAN ONE ACTION SELECTED');
      return;
    }
    
    const [cardNumber, action] = selectedCards[0];
    
    // Navigate based on action
    if (action === 'S') {
      router.push(`/cards/${cardNumber}`);
    } else if (action === 'U') {
      router.push(`/cards/${cardNumber}/edit`);
    }
  };

  /**
   * Handle previous page (PF7)
   */
  const handlePreviousPage = () => {
    if (pageInfo.isFirst) {
      setError('NO PREVIOUS PAGES TO DISPLAY');
      return;
    }
    fetchCards(pageInfo.currentPage - 1);
  };

  /**
   * Handle next page (PF8)
   */
  const handleNextPage = () => {
    if (pageInfo.isLast) {
      setError('NO MORE PAGES TO DISPLAY');
      return;
    }
    fetchCards(pageInfo.currentPage + 1);
  };

  /**
   * Handle exit to menu (PF3)
   */
  const handleExit = () => {
    router.push('/');
  };

  /**
   * Handle keyboard shortcuts
   */
  const handleKeyDown = useCallback(
    (e: KeyboardEvent) => {
      // F3 - Exit
      if (e.key === 'F3') {
        e.preventDefault();
        handleExit();
      }
      // F7 - Previous page
      else if (e.key === 'F7') {
        e.preventDefault();
        handlePreviousPage();
      }
      // F8 - Next page
      else if (e.key === 'F8') {
        e.preventDefault();
        handleNextPage();
      }
    },
    [pageInfo]
  );

  useEffect(() => {
    window.addEventListener('keydown', handleKeyDown);
    return () => window.removeEventListener('keydown', handleKeyDown);
  }, [handleKeyDown]);

  if (loading && cards.length === 0) {
    return (
      <div className="p-6 flex items-center justify-center min-h-screen">
        <div className="text-lg">Loading...</div>
      </div>
    );
  }

  return (
    <div className="p-6 max-w-7xl mx-auto">
      {/* Header Section */}
      <div className="bg-blue-900 text-white p-4 mb-6 rounded-lg">
        <div className="flex justify-between items-center">
          <div>
            <h1 className="text-2xl font-bold">CREDIT CARD LIST</h1>
            <p className="text-sm">Transaction: CCRDLIA | Program: COCRDLIC</p>
          </div>
          <div className="text-right">
            <p className="text-sm">Date: {currentDateTime.date}</p>
            <p className="text-sm">Time: {currentDateTime.time}</p>
            <p className="text-sm">
              Page: {pageInfo.currentPage + 1} of {pageInfo.totalPages || 1}
            </p>
          </div>
        </div>
      </div>

      {/* Search Criteria Section */}
      <div className="bg-white shadow rounded-lg p-6 mb-6">
        <h2 className="text-lg font-semibold mb-4">Search Criteria</h2>
        <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
          <Input
            label="Account ID (11 digits)"
            value={accountIdFilter}
            onChange={(e) => {
              setAccountIdFilter(e.target.value);
              setError(null);
            }}
            placeholder="Enter 11-digit account ID"
            maxLength={11}
          />
          <Input
            label="Card Number (16 digits)"
            value={cardNumberFilter}
            onChange={(e) => {
              setCardNumberFilter(e.target.value);
              setError(null);
            }}
            placeholder="Enter 16-digit card number"
            maxLength={16}
          />
        </div>
        <div className="mt-4 flex gap-2">
          <Button onClick={handleApplyFilters}>Apply Filters</Button>
          <Button
            variant="secondary"
            onClick={() => {
              setAccountIdFilter('');
              setCardNumberFilter('');
              setError(null);
              fetchCards(0);
            }}
          >
            Clear Filters
          </Button>
        </div>
      </div>

      {/* Error/Info Messages Section */}
      {error && (
        <div
          className={`p-4 mb-6 rounded-lg ${
            error.includes('NO RECORDS') || error.includes('NO MORE') || error.includes('NO PREVIOUS')
              ? 'bg-blue-100 text-blue-800'
              : 'bg-red-100 text-red-800'
          }`}
        >
          {error}
        </div>
      )}

      {/* Card List Grid Section */}
      <div className="bg-white shadow rounded-lg overflow-hidden mb-6">
        <div className="overflow-x-auto">
          <table className="min-w-full divide-y divide-gray-200">
            <thead className="bg-gray-50">
              <tr>
                <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider w-24">
                  Action
                </th>
                <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                  Account Number
                </th>
                <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                  Card Number
                </th>
                <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                  Status
                </th>
              </tr>
            </thead>
            <tbody className="bg-white divide-y divide-gray-200">
              {/* Display up to 7 rows (COBOL screen specification) */}
              {Array.from({ length: 7 }).map((_, index) => {
                const card = cards[index];
                if (!card) {
                  // Empty row
                  return (
                    <tr key={`empty-${index}`} className="h-12">
                      <td className="px-6 py-4">&nbsp;</td>
                      <td className="px-6 py-4">&nbsp;</td>
                      <td className="px-6 py-4">&nbsp;</td>
                      <td className="px-6 py-4">&nbsp;</td>
                    </tr>
                  );
                }

                const hasError =
                  error &&
                  (error.includes('MORE THAN ONE') ||
                    error.includes('INVALID ACTION')) &&
                  selections[card.cardNumber];

                return (
                  <tr
                    key={card.cardNumber}
                    className={hasError ? 'bg-red-50' : 'hover:bg-gray-50'}
                  >
                    <td className="px-6 py-4 whitespace-nowrap">
                      <input
                        type="text"
                        value={selections[card.cardNumber] || ''}
                        onChange={(e) =>
                          handleSelectionChange(card.cardNumber, e.target.value)
                        }
                        maxLength={1}
                        className={`w-12 px-2 py-1 border rounded text-center uppercase ${
                          hasError
                            ? 'border-red-500 bg-red-50'
                            : 'border-gray-300'
                        }`}
                        placeholder="S/U"
                      />
                    </td>
                    <td className="px-6 py-4 whitespace-nowrap font-mono">
                      {card.accountId}
                    </td>
                    <td className="px-6 py-4 whitespace-nowrap font-mono">
                      {card.cardNumber}
                    </td>
                    <td className="px-6 py-4 whitespace-nowrap">
                      <span
                        className={`px-2 py-1 text-xs font-semibold rounded ${
                          card.cardStatus === 'Y'
                            ? 'bg-green-100 text-green-800'
                            : 'bg-gray-100 text-gray-800'
                        }`}
                      >
                        {card.cardStatus === 'Y' ? 'Active' : 'Inactive'}
                      </span>
                    </td>
                  </tr>
                );
              })}
            </tbody>
          </table>
        </div>
      </div>

      {/* Action Instructions */}
      <div className="bg-gray-50 p-4 rounded-lg mb-6">
        <p className="text-sm text-gray-700">
          <strong>Instructions:</strong> Enter 'S' to view card details or 'U'
          to update card, then press Enter or click Execute Action.
        </p>
      </div>

      {/* Navigation Buttons */}
      <div className="flex justify-between items-center">
        <div className="flex gap-2">
          <Button onClick={handleExecuteAction} disabled={loading}>
            Execute Action (Enter)
          </Button>
          <Button variant="secondary" onClick={handleExit}>
            Exit (F3)
          </Button>
        </div>
        <div className="flex gap-2">
          <Button
            variant="secondary"
            onClick={handlePreviousPage}
            disabled={pageInfo.isFirst || loading}
          >
            Previous (F7)
          </Button>
          <Button
            variant="secondary"
            onClick={handleNextPage}
            disabled={pageInfo.isLast || loading}
          >
            Next (F8)
          </Button>
        </div>
      </div>

      {/* Footer Info */}
      <div className="mt-6 text-center text-sm text-gray-600">
        <p>
          Showing {cards.length} of {pageInfo.totalElements} total cards
        </p>
      </div>
    </div>
  );
}
