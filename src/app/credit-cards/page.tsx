'use client';

import React, { useEffect, useState } from 'react';
import { useRouter } from 'next/navigation';
import { creditCardService } from '@/services/creditCardService';
import {
  CreditCard,
  CreditCardFilterRequest,
  ERROR_MESSAGES,
  CARD_STATUS_OPTIONS,
} from '@/types/credit-card';
import { Table, Button, Input, Select } from '@/components/ui';

export default function CreditCardsPage() {
  const router = useRouter();
  const [creditCards, setCreditCards] = useState<CreditCard[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [currentPage, setCurrentPage] = useState(0);
  const [totalPages, setTotalPages] = useState(0);
  const [isFirstPage, setIsFirstPage] = useState(true);
  const [isLastPage, setIsLastPage] = useState(true);
  const [pageSize] = useState(20);
  
  // Filter state
  const [accountIdFilter, setAccountIdFilter] = useState('');
  const [cardNumberFilter, setCardNumberFilter] = useState('');
  const [cardStatusFilter, setCardStatusFilter] = useState('');
  
  // Validation errors
  const [accountIdError, setAccountIdError] = useState<string | null>(null);
  const [cardNumberError, setCardNumberError] = useState<string | null>(null);
  
  // User context (in production, this would come from AuthContext)
  const [userId] = useState('USER001'); // Default user for demo
  const [currentDateTime, setCurrentDateTime] = useState(new Date());

  useEffect(() => {
    fetchCreditCards();
    
    // Update time every second
    const timer = setInterval(() => {
      setCurrentDateTime(new Date());
    }, 1000);
    
    return () => clearInterval(timer);
  }, []);

  const fetchCreditCards = async (
    page: number = 0,
    filters?: CreditCardFilterRequest
  ) => {
    try {
      setLoading(true);
      setError(null);
      
      const filterRequest: CreditCardFilterRequest = {
        accountId: filters?.accountId || accountIdFilter || undefined,
        cardNumber: filters?.cardNumber || cardNumberFilter || undefined,
        cardStatus: filters?.cardStatus || cardStatusFilter || undefined,
        page,
        size: pageSize,
      };

      const data = await creditCardService.filterCreditCards(
        userId,
        filterRequest
      );
      
      setCreditCards(data.content);
      setCurrentPage(data.number);
      setTotalPages(data.totalPages);
      setIsFirstPage(data.first);
      setIsLastPage(data.last);
      
      if (data.content.length === 0) {
        setError(ERROR_MESSAGES.NO_RECORDS_FOUND);
      }
    } catch (err) {
      setError('Failed to load credit cards');
      console.error(err);
    } finally {
      setLoading(false);
    }
  };

  const handleApplyFilters = () => {
    // Clear previous errors
    setAccountIdError(null);
    setCardNumberError(null);
    
    // Validate account ID filter
    if (accountIdFilter) {
      const accountValidation = creditCardService.validateAccountFilter(
        accountIdFilter
      );
      if (!accountValidation.isValid) {
        setAccountIdError(accountValidation.error || 'Invalid account ID');
        return;
      }
    }
    
    // Validate card number filter
    if (cardNumberFilter) {
      const cardValidation = creditCardService.validateCardNumberFilter(
        cardNumberFilter
      );
      if (!cardValidation.isValid) {
        setCardNumberError(cardValidation.error || 'Invalid card number');
        return;
      }
    }
    
    // Apply filters
    fetchCreditCards(0);
  };

  const handleClearFilters = () => {
    setAccountIdFilter('');
    setCardNumberFilter('');
    setCardStatusFilter('');
    setAccountIdError(null);
    setCardNumberError(null);
    setError(null);
    fetchCreditCards(0, {});
  };

  const handlePreviousPage = async () => {
    if (isFirstPage) {
      setError(ERROR_MESSAGES.NO_PREVIOUS_PAGES);
      return;
    }
    
    try {
      await creditCardService.validateBackwardNavigation(currentPage);
      fetchCreditCards(currentPage - 1);
    } catch (err) {
      setError(ERROR_MESSAGES.NO_PREVIOUS_PAGES);
    }
  };

  const handleNextPage = async () => {
    if (isLastPage) {
      setError(ERROR_MESSAGES.NO_MORE_PAGES);
      return;
    }
    
    try {
      await creditCardService.validateForwardNavigation({ last: isLastPage });
      fetchCreditCards(currentPage + 1);
    } catch (err) {
      setError(ERROR_MESSAGES.NO_MORE_PAGES);
    }
  };

  const handleViewDetails = (card: CreditCard) => {
    // Navigate to card detail view (COCRDSLC)
    router.push(`/credit-cards/${card.cardNumber}`);
  };

  const handleUpdateCard = (card: CreditCard) => {
    // Navigate to card update (COCRDUPC)
    router.push(`/credit-cards/${card.cardNumber}/edit`);
  };

  const handleExitToMenu = () => {
    // Navigate to main menu (COMEN01C)
    router.push('/');
  };

  const formatDate = (date: Date): string => {
    return date.toLocaleDateString('en-US', {
      year: 'numeric',
      month: '2-digit',
      day: '2-digit',
    });
  };

  const formatTime = (date: Date): string => {
    return date.toLocaleTimeString('en-US', {
      hour: '2-digit',
      minute: '2-digit',
      second: '2-digit',
      hour12: false,
    });
  };

  if (loading && creditCards.length === 0) {
    return (
      <div className="flex items-center justify-center min-h-screen">
        <div className="text-lg">Loading credit cards...</div>
      </div>
    );
  }

  return (
    <div className="min-h-screen bg-gray-50 p-6">
      {/* Header Section */}
      <div className="bg-white shadow rounded-lg p-6 mb-6">
        <div className="flex justify-between items-start mb-4">
          <div>
            <h1 className="text-3xl font-bold text-gray-900">
              Credit Card List
            </h1>
            <p className="text-sm text-gray-600 mt-1">
              Transaction ID: CCRDLIA | Program: COCRDLIC
            </p>
          </div>
          <div className="text-right text-sm text-gray-600">
            <div>{formatDate(currentDateTime)}</div>
            <div>{formatTime(currentDateTime)}</div>
            <div className="mt-1">
              Page {currentPage + 1} of {totalPages || 1}
            </div>
          </div>
        </div>

        {/* Filter Criteria Section */}
        <div className="border-t pt-4">
          <h2 className="text-lg font-semibold mb-4">Filter Criteria</h2>
          <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
            <div>
              <Input
                label="Account ID (11 digits)"
                value={accountIdFilter}
                onChange={(e) => {
                  setAccountIdFilter(e.target.value);
                  setAccountIdError(null);
                }}
                placeholder="12345678901"
                maxLength={11}
                error={accountIdError || undefined}
              />
            </div>
            <div>
              <Input
                label="Card Number (16 digits)"
                value={cardNumberFilter}
                onChange={(e) => {
                  setCardNumberFilter(e.target.value);
                  setCardNumberError(null);
                }}
                placeholder="1234567890123456"
                maxLength={16}
                error={cardNumberError || undefined}
              />
            </div>
            <div>
              <Select
                label="Card Status"
                value={cardStatusFilter}
                onChange={(e) => setCardStatusFilter(e.target.value)}
                options={[
                  { value: '', label: 'All Statuses' },
                  ...CARD_STATUS_OPTIONS,
                ]}
              />
            </div>
          </div>
          <div className="flex gap-2 mt-4">
            <Button onClick={handleApplyFilters}>Apply Filters</Button>
            <Button variant="secondary" onClick={handleClearFilters}>
              Clear Filters
            </Button>
          </div>
        </div>
      </div>

      {/* Error Messages */}
      {error && (
        <div className="bg-red-50 border border-red-200 text-red-800 px-4 py-3 rounded mb-6">
          {error}
        </div>
      )}

      {/* Card List Grid Section */}
      <div className="bg-white shadow rounded-lg p-6 mb-6">
        <div className="flex justify-between items-center mb-4">
          <h2 className="text-lg font-semibold">Credit Cards</h2>
          <div className="text-sm text-gray-600">
            {creditCards.length} card(s) displayed
          </div>
        </div>

        {creditCards.length === 0 ? (
          <div className="text-center py-12 text-gray-500">
            {error || 'No credit cards found'}
          </div>
        ) : (
          <Table
            columns={[
              {
                key: 'accountId',
                label: 'Account Number',
              },
              {
                key: 'formattedCardNumber',
                label: 'Card Number',
              },
              {
                key: 'cardStatusDisplayName',
                label: 'Status',
              },
              {
                key: 'isActive',
                label: 'Active',
                render: (value) => (
                  <span
                    className={`px-2 py-1 rounded text-xs font-semibold ${
                      value
                        ? 'bg-green-100 text-green-800'
                        : 'bg-gray-100 text-gray-800'
                    }`}
                  >
                    {value ? 'Yes' : 'No'}
                  </span>
                ),
              },
            ]}
            data={creditCards}
            onRowClick={(card) => handleViewDetails(card)}
            actions={(card) => (
              <div className="flex gap-2">
                <Button
                  size="sm"
                  onClick={(e) => {
                    e.stopPropagation();
                    handleViewDetails(card);
                  }}
                  title="View card details"
                >
                  View
                </Button>
                <Button
                  size="sm"
                  variant="secondary"
                  onClick={(e) => {
                    e.stopPropagation();
                    handleUpdateCard(card);
                  }}
                  title="Update card information"
                >
                  Update
                </Button>
              </div>
            )}
          />
        )}
      </div>

      {/* Navigation Controls */}
      <div className="bg-white shadow rounded-lg p-6">
        <div className="flex justify-between items-center">
          <div className="flex gap-2">
            <Button
              variant="secondary"
              onClick={handlePreviousPage}
              disabled={isFirstPage || loading}
            >
              ← Previous Page
            </Button>
            <Button
              variant="secondary"
              onClick={handleNextPage}
              disabled={isLastPage || loading}
            >
              Next Page →
            </Button>
          </div>
          <div className="text-sm text-gray-600">
            Page {currentPage + 1} of {totalPages || 1}
          </div>
          <Button variant="secondary" onClick={handleExitToMenu}>
            Exit to Menu
          </Button>
        </div>
      </div>
    </div>
  );
}
