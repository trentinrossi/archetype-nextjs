'use client';

import React, { useEffect, useState } from 'react';
import { useRouter } from 'next/navigation';
import { cardService } from '@/services/cardService';
import { CardListItem, SelectionCode } from '@/types/card';
import { Button, Input } from '@/components/ui';

/**
 * Credit Card List Screen (CCRDLIA)
 * Displays paginated list of credit cards with filtering and selection capabilities
 * 
 * Business Rules:
 * - Account ID filter must be 11 digits if supplied
 * - Card number filter must be 16 digits if supplied
 * - Only one card can be selected at a time
 * - Selection code 'S' navigates to detail view
 * - Selection code 'U' navigates to update screen
 * - Page size is 7 records per page
 * - PF3 returns to main menu
 * - PF7 goes to previous page
 * - PF8 goes to next page
 */
export default function CardsPage() {
  const router = useRouter();
  
  // Filter state
  const [accountIdFilter, setAccountIdFilter] = useState('');
  const [cardNumberFilter, setCardNumberFilter] = useState('');
  
  // Data state
  const [cards, setCards] = useState<CardListItem[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  
  // Pagination state
  const [currentPage, setCurrentPage] = useState(0);
  const [totalPages, setTotalPages] = useState(0);
  const [totalElements, setTotalElements] = useState(0);
  const [isFirstPage, setIsFirstPage] = useState(true);
  const [isLastPage, setIsLastPage] = useState(true);
  
  // Validation state
  const [accountIdError, setAccountIdError] = useState('');
  const [cardNumberError, setCardNumberError] = useState('');
  
  // Message state
  const [infoMessage, setInfoMessage] = useState('');
  
  const PAGE_SIZE = 7;

  useEffect(() => {
    fetchCards();
  }, [currentPage]);

  /**
   * Fetch cards with current filters and pagination
   */
  const fetchCards = async () => {
    try {
      setLoading(true);
      setError(null);
      setInfoMessage('');
      
      const criteria = {
        page: currentPage,
        size: PAGE_SIZE,
        ...(accountIdFilter && { accountId: accountIdFilter }),
        ...(cardNumberFilter && { cardNumber: cardNumberFilter }),
      };
      
      const response = await cardService.getCardsList(criteria);
      
      // Transform response to include selection code
      const cardsWithSelection: CardListItem[] = response.content.map(card => ({
        ...card,
        selectionCode: '' as SelectionCode,
      }));
      
      setCards(cardsWithSelection);
      setTotalPages(response.totalPages);
      setTotalElements(response.totalElements);
      setIsFirstPage(response.first);
      setIsLastPage(response.last);
      
      // Display message if no records found
      if (response.empty || response.totalElements === 0) {
        setInfoMessage('NO RECORDS FOUND FOR THIS SEARCH CONDITION');
      }
    } catch (err) {
      setError('Failed to load cards');
      console.error(err);
    } finally {
      setLoading(false);
    }
  };

  /**
   * Validate account ID filter
   * Must be 11 digits if supplied
   */
  const validateAccountId = (value: string): boolean => {
    if (!value) {
      setAccountIdError('');
      return true;
    }
    
    if (!/^\d{11}$/.test(value)) {
      setAccountIdError('ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER');
      return false;
    }
    
    setAccountIdError('');
    return true;
  };

  /**
   * Validate card number filter
   * Must be 16 digits if supplied
   */
  const validateCardNumber = (value: string): boolean => {
    if (!value) {
      setCardNumberError('');
      return true;
    }
    
    if (!/^\d{16}$/.test(value)) {
      setCardNumberError('CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMBER');
      return false;
    }
    
    setCardNumberError('');
    return true;
  };

  /**
   * Handle filter change for account ID
   */
  const handleAccountIdChange = (value: string) => {
    setAccountIdFilter(value);
    validateAccountId(value);
  };

  /**
   * Handle filter change for card number
   */
  const handleCardNumberChange = (value: string) => {
    setCardNumberFilter(value);
    validateCardNumber(value);
  };

  /**
   * Handle search/filter submission
   */
  const handleSearch = () => {
    // Validate filters
    const isAccountIdValid = validateAccountId(accountIdFilter);
    const isCardNumberValid = validateCardNumber(cardNumberFilter);
    
    if (!isAccountIdValid || !isCardNumberValid) {
      return;
    }
    
    // Reset to first page and fetch
    setCurrentPage(0);
    fetchCards();
  };

  /**
   * Handle selection code change for a card
   */
  const handleSelectionChange = (index: number, value: string) => {
    const upperValue = value.toUpperCase();
    
    // Validate selection code
    if (upperValue && upperValue !== 'S' && upperValue !== 'U') {
      setError('Invalid action code');
      return;
    }
    
    setError(null);
    
    const updatedCards = [...cards];
    updatedCards[index] = {
      ...updatedCards[index],
      selectionCode: upperValue as SelectionCode,
    };
    setCards(updatedCards);
  };

  /**
   * Handle Enter key press (process selection)
   */
  const handleEnter = () => {
    // Count selected cards
    const selectedCards = cards.filter(card => card.selectionCode);
    
    if (selectedCards.length === 0) {
      setError('No card selected');
      return;
    }
    
    if (selectedCards.length > 1) {
      setError('Only one selection allowed');
      return;
    }
    
    const selectedCard = selectedCards[0];
    
    // Navigate based on selection code
    if (selectedCard.selectionCode === 'S') {
      // Navigate to detail view
      router.push(`/cards/${selectedCard.cardNumber}`);
    } else if (selectedCard.selectionCode === 'U') {
      // Navigate to update screen
      router.push(`/cards/${selectedCard.cardNumber}/edit`);
    }
  };

  /**
   * Handle PF3 - Exit to main menu
   */
  const handleExit = () => {
    router.push('/');
  };

  /**
   * Handle PF7 - Previous page
   */
  const handlePreviousPage = () => {
    if (isFirstPage) {
      setInfoMessage('NO PREVIOUS PAGES TO DISPLAY');
      return;
    }
    
    setCurrentPage(prev => Math.max(0, prev - 1));
  };

  /**
   * Handle PF8 - Next page
   */
  const handleNextPage = () => {
    if (isLastPage) {
      setInfoMessage('NO MORE PAGES TO DISPLAY');
      return;
    }
    
    setCurrentPage(prev => Math.min(totalPages - 1, prev + 1));
  };

  /**
   * Handle keyboard shortcuts
   */
  useEffect(() => {
    const handleKeyDown = (e: KeyboardEvent) => {
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
    };

    window.addEventListener('keydown', handleKeyDown);
    return () => window.removeEventListener('keydown', handleKeyDown);
  }, [isFirstPage, isLastPage, currentPage]);

  // Get current date and time for header
  const currentDate = new Date().toLocaleDateString();
  const currentTime = new Date().toLocaleTimeString();

  if (loading && cards.length === 0) {
    return <div className="p-6">Loading...</div>;
  }

  return (
    <div className="p-6 max-w-7xl mx-auto">
      {/* Header Section */}
      <div className="mb-6 pb-4 border-b border-gray-300">
        <div className="flex justify-between items-start">
          <div>
            <h1 className="text-2xl font-bold text-gray-900">Credit Card List</h1>
            <p className="text-sm text-gray-600 mt-1">CCRDLIA - Card List Program</p>
          </div>
          <div className="text-right text-sm text-gray-600">
            <div>{currentDate}</div>
            <div>{currentTime}</div>
            <div>Page {currentPage + 1} of {totalPages || 1}</div>
          </div>
        </div>
      </div>

      {/* Search Criteria Section */}
      <div className="mb-6 bg-gray-50 p-4 rounded-lg border border-gray-200">
        <h2 className="text-lg font-semibold mb-4 text-gray-900">Search Criteria</h2>
        <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
          <div>
            <Input
              label="Account ID (11 digits)"
              value={accountIdFilter}
              onChange={(e) => handleAccountIdChange(e.target.value)}
              placeholder="Enter 11-digit account ID"
              error={accountIdError}
              maxLength={11}
            />
          </div>
          <div>
            <Input
              label="Card Number (16 digits)"
              value={cardNumberFilter}
              onChange={(e) => handleCardNumberChange(e.target.value)}
              placeholder="Enter 16-digit card number"
              error={cardNumberError}
              maxLength={16}
            />
          </div>
        </div>
        <div className="mt-4">
          <Button onClick={handleSearch} disabled={loading}>
            {loading ? 'Searching...' : 'Search'}
          </Button>
        </div>
      </div>

      {/* Messages Section */}
      {(error || infoMessage) && (
        <div className="mb-4">
          {error && (
            <div className="bg-red-50 border border-red-300 text-red-800 px-4 py-3 rounded">
              {error}
            </div>
          )}
          {infoMessage && (
            <div className="bg-blue-50 border border-blue-300 text-blue-800 px-4 py-3 rounded">
              {infoMessage}
            </div>
          )}
        </div>
      )}

      {/* Card List Section */}
      <div className="bg-white shadow rounded-lg overflow-hidden">
        <div className="overflow-x-auto">
          <table className="min-w-full divide-y divide-gray-200">
            <thead className="bg-gray-100">
              <tr>
                <th className="px-4 py-3 text-left text-xs font-semibold text-gray-700 uppercase tracking-wider w-24">
                  Select
                </th>
                <th className="px-4 py-3 text-left text-xs font-semibold text-gray-700 uppercase tracking-wider">
                  Account Number
                </th>
                <th className="px-4 py-3 text-left text-xs font-semibold text-gray-700 uppercase tracking-wider">
                  Card Number
                </th>
                <th className="px-4 py-3 text-left text-xs font-semibold text-gray-700 uppercase tracking-wider">
                  Status
                </th>
              </tr>
            </thead>
            <tbody className="bg-white divide-y divide-gray-200">
              {cards.length === 0 ? (
                <tr>
                  <td colSpan={4} className="px-4 py-8 text-center text-gray-500">
                    No cards found
                  </td>
                </tr>
              ) : (
                cards.map((card, index) => (
                  <tr
                    key={card.cardNumber}
                    className={`hover:bg-gray-50 ${
                      card.selectionCode ? 'bg-blue-50' : ''
                    }`}
                  >
                    <td className="px-4 py-3">
                      <input
                        type="text"
                        value={card.selectionCode}
                        onChange={(e) => handleSelectionChange(index, e.target.value)}
                        maxLength={1}
                        className="w-12 px-2 py-1 border border-gray-300 rounded text-center uppercase focus:outline-none focus:ring-2 focus:ring-blue-500"
                        placeholder="_"
                      />
                    </td>
                    <td className="px-4 py-3 text-sm text-gray-900">
                      {card.accountId}
                    </td>
                    <td className="px-4 py-3 text-sm text-gray-900 font-mono">
                      {card.cardNumber}
                    </td>
                    <td className="px-4 py-3">
                      <span
                        className={`inline-flex px-2 py-1 text-xs font-semibold rounded-full ${
                          card.activeStatus === 'Y'
                            ? 'bg-green-100 text-green-800'
                            : 'bg-red-100 text-red-800'
                        }`}
                      >
                        {card.activeStatus === 'Y' ? 'Active' : 'Inactive'}
                      </span>
                    </td>
                  </tr>
                ))
              )}
            </tbody>
          </table>
        </div>
      </div>

      {/* Action Buttons Section */}
      <div className="mt-6 flex flex-wrap gap-3">
        <Button onClick={handleEnter} disabled={loading}>
          Enter - Process Selection
        </Button>
        <Button onClick={handlePreviousPage} variant="secondary" disabled={loading || isFirstPage}>
          F7 - Previous Page
        </Button>
        <Button onClick={handleNextPage} variant="secondary" disabled={loading || isLastPage}>
          F8 - Next Page
        </Button>
        <Button onClick={handleExit} variant="secondary">
          F3 - Exit
        </Button>
      </div>

      {/* Pagination Info */}
      <div className="mt-4 text-sm text-gray-600 text-center">
        Showing {cards.length} of {totalElements} total cards
        {totalPages > 1 && ` (Page ${currentPage + 1} of ${totalPages})`}
      </div>

      {/* Instructions */}
      <div className="mt-6 p-4 bg-gray-50 rounded-lg border border-gray-200">
        <h3 className="text-sm font-semibold text-gray-900 mb-2">Instructions:</h3>
        <ul className="text-sm text-gray-700 space-y-1">
          <li>• Enter <strong>S</strong> in the Select column to view card details</li>
          <li>• Enter <strong>U</strong> in the Select column to update card information</li>
          <li>• Only one card can be selected at a time</li>
          <li>• Press <strong>Enter</strong> or click the button to process your selection</li>
          <li>• Use <strong>F7</strong> and <strong>F8</strong> (or buttons) to navigate pages</li>
          <li>• Press <strong>F3</strong> (or Exit button) to return to main menu</li>
        </ul>
      </div>
    </div>
  );
}
