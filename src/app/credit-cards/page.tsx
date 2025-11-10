'use client';

import React, { useEffect, useState } from 'react';
import { useRouter } from 'next/navigation';
import { creditCardService } from '@/services/creditCardService';
import { CreditCard, CreditCardSearchFilters } from '@/types/creditCard';
import { Table, Button, Input, Select } from '@/components/ui';

export default function CreditCardsPage() {
  const router = useRouter();
  const [creditCards, setCreditCards] = useState<CreditCard[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [currentPage, setCurrentPage] = useState(0);
  const [totalPages, setTotalPages] = useState(0);
  const [totalElements, setTotalElements] = useState(0);
  const [pageSize] = useState(20);
  
  // Search filters
  const [accountIdFilter, setAccountIdFilter] = useState('');
  const [cardNumberFilter, setCardNumberFilter] = useState('');
  const [cardStatusFilter, setCardStatusFilter] = useState('');
  
  // Validation errors
  const [accountIdError, setAccountIdError] = useState('');
  const [cardNumberError, setCardNumberError] = useState('');
  
  // Info messages
  const [infoMessage, setInfoMessage] = useState('');

  useEffect(() => {
    fetchCreditCards();
  }, [currentPage]);

  const validateFilters = (): boolean => {
    let isValid = true;
    setAccountIdError('');
    setCardNumberError('');
    
    // Validate account ID filter if provided
    if (accountIdFilter && !/^\d{11}$/.test(accountIdFilter)) {
      setAccountIdError('ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER');
      isValid = false;
    }
    
    // Validate card number filter if provided
    if (cardNumberFilter && !/^\d{16}$/.test(cardNumberFilter)) {
      setCardNumberError('CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMBER');
      isValid = false;
    }
    
    return isValid;
  };

  const fetchCreditCards = async () => {
    try {
      setLoading(true);
      setError(null);
      setInfoMessage('');
      
      const filters: CreditCardSearchFilters = {};
      
      if (accountIdFilter) {
        filters.accountId = accountIdFilter;
      }
      
      if (cardNumberFilter) {
        filters.cardNumberPattern = cardNumberFilter;
      }
      
      if (cardStatusFilter) {
        filters.cardStatus = cardStatusFilter;
      }
      
      const data = await creditCardService.searchCreditCards(filters, currentPage, pageSize);
      setCreditCards(data.content);
      setTotalPages(data.totalPages);
      setTotalElements(data.totalElements);
      
      // Display message if no records found
      if (data.content.length === 0) {
        setInfoMessage('NO RECORDS FOUND FOR THIS SEARCH CONDITION');
      }
    } catch (err) {
      setError('Failed to load credit cards');
      console.error(err);
    } finally {
      setLoading(false);
    }
  };

  const handleSearch = () => {
    if (!validateFilters()) {
      return;
    }
    
    // Reset to first page when searching
    setCurrentPage(0);
    fetchCreditCards();
  };

  const handleClearFilters = () => {
    setAccountIdFilter('');
    setCardNumberFilter('');
    setCardStatusFilter('');
    setAccountIdError('');
    setCardNumberError('');
    setInfoMessage('');
    setCurrentPage(0);
    fetchCreditCards();
  };

  const handlePreviousPage = () => {
    if (currentPage === 0) {
      setInfoMessage('NO PREVIOUS PAGES TO DISPLAY');
      return;
    }
    setCurrentPage(currentPage - 1);
  };

  const handleNextPage = () => {
    if (currentPage >= totalPages - 1) {
      setInfoMessage('NO MORE PAGES TO DISPLAY');
      return;
    }
    setCurrentPage(currentPage + 1);
  };

  const handleViewCard = (cardNumber: string) => {
    router.push(`/credit-cards/${cardNumber}`);
  };

  const handleEditCard = (cardNumber: string) => {
    router.push(`/credit-cards/${cardNumber}/edit`);
  };

  const handleDeleteCard = async (cardNumber: string) => {
    if (!confirm('Are you sure you want to delete this credit card?')) return;
    
    try {
      await creditCardService.deleteCreditCard(cardNumber);
      fetchCreditCards();
    } catch (err) {
      alert('Failed to delete credit card');
      console.error(err);
    }
  };

  const getStatusBadgeColor = (status: string) => {
    switch (status) {
      case 'A':
        return 'bg-green-100 text-green-800';
      case 'I':
        return 'bg-gray-100 text-gray-800';
      case 'B':
        return 'bg-red-100 text-red-800';
      case 'C':
        return 'bg-black text-white';
      case 'S':
        return 'bg-yellow-100 text-yellow-800';
      default:
        return 'bg-gray-100 text-gray-800';
    }
  };

  const formatCardNumber = (cardNumber: string) => {
    // Format as XXXX-XXXX-XXXX-XXXX
    return cardNumber.replace(/(\d{4})(?=\d)/g, '$1-');
  };

  if (loading && creditCards.length === 0) {
    return (
      <div className="p-6">
        <div className="flex items-center justify-center h-64">
          <div className="text-lg">Loading credit cards...</div>
        </div>
      </div>
    );
  }

  return (
    <div className="p-6">
      {/* Header */}
      <div className="mb-6">
        <div className="flex justify-between items-center mb-2">
          <h1 className="text-2xl font-bold">Credit Card List</h1>
          <div className="text-sm text-gray-600">
            {new Date().toLocaleDateString()} {new Date().toLocaleTimeString()}
          </div>
        </div>
        <div className="text-sm text-gray-600">
          Page {currentPage + 1} of {totalPages || 1} | Total Records: {totalElements}
        </div>
      </div>

      {/* Search Criteria Section */}
      <div className="bg-white shadow rounded-lg p-6 mb-6">
        <h2 className="text-lg font-semibold mb-4">Search Criteria</h2>
        <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
          <div>
            <Input
              label="Account ID (11 digits)"
              value={accountIdFilter}
              onChange={(e) => {
                setAccountIdFilter(e.target.value);
                setAccountIdError('');
              }}
              placeholder="12345678901"
              maxLength={11}
            />
            {accountIdError && (
              <div className="text-red-600 text-sm mt-1">{accountIdError}</div>
            )}
          </div>
          
          <div>
            <Input
              label="Card Number (16 digits)"
              value={cardNumberFilter}
              onChange={(e) => {
                setCardNumberFilter(e.target.value);
                setCardNumberError('');
              }}
              placeholder="1234567890123456"
              maxLength={16}
            />
            {cardNumberError && (
              <div className="text-red-600 text-sm mt-1">{cardNumberError}</div>
            )}
          </div>
          
          <div>
            <Select
              label="Card Status"
              value={cardStatusFilter}
              onChange={(e) => setCardStatusFilter(e.target.value)}
              options={[
                { value: '', label: 'All Statuses' },
                { value: 'A', label: 'Active' },
                { value: 'I', label: 'Inactive' },
                { value: 'B', label: 'Blocked' },
                { value: 'C', label: 'Cancelled' },
                { value: 'S', label: 'Suspended' },
              ]}
            />
          </div>
        </div>
        
        <div className="flex gap-2 mt-4">
          <Button onClick={handleSearch}>
            Search
          </Button>
          <Button variant="secondary" onClick={handleClearFilters}>
            Clear Filters
          </Button>
          <Button onClick={() => router.push('/credit-cards/new')}>
            Create New Card
          </Button>
        </div>
      </div>

      {/* Error Message */}
      {error && (
        <div className="bg-red-50 border border-red-200 text-red-700 px-4 py-3 rounded mb-4">
          {error}
        </div>
      )}

      {/* Info Message */}
      {infoMessage && (
        <div className="bg-blue-50 border border-blue-200 text-blue-700 px-4 py-3 rounded mb-4">
          {infoMessage}
        </div>
      )}

      {/* Card List Table */}
      <div className="bg-white shadow rounded-lg overflow-hidden">
        {creditCards.length === 0 ? (
          <div className="p-8 text-center text-gray-500">
            {infoMessage || 'No credit cards found'}
          </div>
        ) : (
          <>
            <div className="overflow-x-auto">
              <table className="min-w-full divide-y divide-gray-200">
                <thead className="bg-gray-50">
                  <tr>
                    <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                      Account Number
                    </th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                      Card Number
                    </th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                      Cardholder Name
                    </th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                      Status
                    </th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                      Expiry
                    </th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                      Credit Limit
                    </th>
                    <th className="px-6 py-3 text-right text-xs font-medium text-gray-500 uppercase tracking-wider">
                      Actions
                    </th>
                  </tr>
                </thead>
                <tbody className="bg-white divide-y divide-gray-200">
                  {creditCards.map((card) => (
                    <tr 
                      key={card.cardNumber}
                      className="hover:bg-gray-50 cursor-pointer"
                      onClick={() => handleViewCard(card.cardNumber)}
                    >
                      <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-900">
                        {card.accountId}
                      </td>
                      <td className="px-6 py-4 whitespace-nowrap text-sm font-mono text-gray-900">
                        {card.maskedCardNumber || formatCardNumber(card.cardNumber)}
                      </td>
                      <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-900">
                        {card.cardholderName || '-'}
                      </td>
                      <td className="px-6 py-4 whitespace-nowrap">
                        <span className={`px-2 py-1 inline-flex text-xs leading-5 font-semibold rounded-full ${getStatusBadgeColor(card.cardStatus)}`}>
                          {card.cardStatusDisplayName}
                        </span>
                      </td>
                      <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-900">
                        {card.expiryMonth && card.expiryYear 
                          ? `${card.expiryMonth}/${card.expiryYear}`
                          : '-'
                        }
                      </td>
                      <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-900">
                        {card.creditLimit 
                          ? `$${card.creditLimit.toLocaleString('en-US', { minimumFractionDigits: 2, maximumFractionDigits: 2 })}`
                          : '-'
                        }
                      </td>
                      <td className="px-6 py-4 whitespace-nowrap text-right text-sm font-medium">
                        <div className="flex gap-2 justify-end">
                          <Button
                            size="sm"
                            onClick={(e) => {
                              e.stopPropagation();
                              handleViewCard(card.cardNumber);
                            }}
                          >
                            View
                          </Button>
                          {card.canModify && (
                            <Button
                              size="sm"
                              onClick={(e) => {
                                e.stopPropagation();
                                handleEditCard(card.cardNumber);
                              }}
                            >
                              Edit
                            </Button>
                          )}
                          <Button
                            size="sm"
                            variant="danger"
                            onClick={(e) => {
                              e.stopPropagation();
                              handleDeleteCard(card.cardNumber);
                            }}
                          >
                            Delete
                          </Button>
                        </div>
                      </td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>

            {/* Pagination Controls */}
            <div className="bg-gray-50 px-6 py-4 flex items-center justify-between border-t border-gray-200">
              <div className="flex-1 flex justify-between items-center">
                <div className="text-sm text-gray-700">
                  Showing {currentPage * pageSize + 1} to {Math.min((currentPage + 1) * pageSize, totalElements)} of {totalElements} results
                </div>
                <div className="flex gap-2">
                  <Button
                    variant="secondary"
                    onClick={handlePreviousPage}
                    disabled={currentPage === 0 || loading}
                  >
                    Previous
                  </Button>
                  <Button
                    variant="secondary"
                    onClick={handleNextPage}
                    disabled={currentPage >= totalPages - 1 || loading}
                  >
                    Next
                  </Button>
                </div>
              </div>
            </div>
          </>
        )}
      </div>

      {/* Action Buttons */}
      <div className="mt-6 flex gap-2">
        <Button variant="secondary" onClick={() => router.push('/')}>
          Back to Main Menu
        </Button>
      </div>
    </div>
  );
}
