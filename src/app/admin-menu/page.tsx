'use client';

import React, { useState, useEffect, useCallback } from 'react';
import { useRouter } from 'next/navigation';
import { userService } from '@/services/userService';
import { menuService } from '@/services/menuService';
import { AdminMenuOption } from '@/types/menu-option';
import { Button } from '@/components/ui';

export default function AdminMenuPage() {
  const router = useRouter();
  const [menuOptions, setMenuOptions] = useState<AdminMenuOption[]>([]);
  const [selectedOption, setSelectedOption] = useState('');
  const [errorMessage, setErrorMessage] = useState('');
  const [successMessage, setSuccessMessage] = useState('');
  const [loading, setLoading] = useState(true);
  const [currentDate, setCurrentDate] = useState('');
  const [currentTime, setCurrentTime] = useState('');

  const fetchMenuOptions = useCallback(async () => {
    try {
      setLoading(true);
      const options = await menuService.getActiveAdminMenuOptions();
      setMenuOptions(options);
    } catch (error) {
      console.error('Failed to load menu options:', error);
      setErrorMessage('Failed to load menu options');
    } finally {
      setLoading(false);
    }
  }, []);

  useEffect(() => {
    const user = userService.getCurrentUser();
    if (!user || user.userType !== 'A') {
      router.push('/login');
      return;
    }

    fetchMenuOptions();
    updateDateTime();
    const interval = setInterval(updateDateTime, 1000);
    return () => clearInterval(interval);
  }, [router, fetchMenuOptions]);

  const updateDateTime = () => {
    const now = new Date();
    setCurrentDate(now.toLocaleDateString('en-US', { 
      month: '2-digit', 
      day: '2-digit', 
      year: 'numeric' 
    }));
    setCurrentTime(now.toLocaleTimeString('en-US', { 
      hour: '2-digit', 
      minute: '2-digit', 
      second: '2-digit',
      hour12: false 
    }));
  };

  const handleOptionSelect = async (optionNumber: number) => {
    setErrorMessage('');
    setSuccessMessage('');

    try {
      const option = await menuService.getAdminMenuOptionByNumber(optionNumber);
      
      if (option.isComingSoon || option.programName.startsWith('DUMMY')) {
        setSuccessMessage('This option is coming soon...');
        return;
      }

      switch (optionNumber) {
        case 1:
          router.push('/users');
          break;
        default:
          setSuccessMessage('This option is coming soon...');
      }
    } catch (error) {
      setErrorMessage('Invalid option selected');
    }
  };

  const handleSubmit = (e: React.FormEvent) => {
    e.preventDefault();
    setErrorMessage('');
    setSuccessMessage('');

    if (!selectedOption.trim()) {
      setErrorMessage('Please enter a valid option number...');
      return;
    }

    const optionNum = parseInt(selectedOption);
    if (isNaN(optionNum) || optionNum < 1 || optionNum > menuOptions.length) {
      setErrorMessage('Please enter a valid option number...');
      return;
    }

    handleOptionSelect(optionNum);
  };

  const handleSignOut = () => {
    userService.logout();
    router.push('/login');
  };

  if (loading) {
    return (
      <div className="min-h-screen bg-gray-50 flex items-center justify-center">
        <div className="text-lg">Loading...</div>
      </div>
    );
  }

  return (
    <div className="min-h-screen bg-gray-50 flex flex-col">
      <div className="bg-blue-900 text-white p-4">
        <div className="max-w-7xl mx-auto flex justify-between items-center">
          <div>
            <h1 className="text-2xl font-bold">AWS Mainframe Modernization</h1>
            <h2 className="text-lg">CardDemo - Admin Menu</h2>
          </div>
          <div className="text-right">
            <div className="text-sm">Date: {currentDate}</div>
            <div className="text-sm">Time: {currentTime}</div>
          </div>
        </div>
      </div>

      <div className="flex-1 p-6">
        <div className="max-w-4xl mx-auto">
          <div className="bg-white shadow-lg rounded-lg p-8">
            <div className="mb-6">
              <div className="text-sm text-gray-600 mb-4">
                <span>Transaction: COADM</span>
                <span className="ml-4">Program: COADM01C</span>
              </div>
              <h3 className="text-xl font-bold text-gray-800">Administrator Menu</h3>
            </div>

            {errorMessage && (
              <div className="mb-4 p-3 bg-red-50 border border-red-200 rounded text-red-700 text-sm">
                {errorMessage}
              </div>
            )}

            {successMessage && (
              <div className="mb-4 p-3 bg-green-50 border border-green-200 rounded text-green-700 text-sm">
                {successMessage}
              </div>
            )}

            <div className="mb-6 space-y-2">
              {menuOptions.map((option) => (
                <div 
                  key={option.id} 
                  className="flex items-center p-3 hover:bg-gray-50 rounded cursor-pointer"
                  onClick={() => handleOptionSelect(option.optionNumber)}
                >
                  <span className="font-semibold text-blue-600 w-12">{option.optionNumber}.</span>
                  <span className="text-gray-800">{option.optionName}</span>
                  {option.isComingSoon && (
                    <span className="ml-auto text-xs text-green-600 font-semibold">Coming Soon</span>
                  )}
                </div>
              ))}
            </div>

            <form onSubmit={handleSubmit} className="space-y-4">
              <div>
                <label className="block text-sm font-medium text-gray-700 mb-2">
                  Select Option
                </label>
                <input
                  type="text"
                  value={selectedOption}
                  onChange={(e) => setSelectedOption(e.target.value)}
                  className="w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500"
                  placeholder="Enter option number"
                  autoFocus
                />
              </div>

              <div className="flex gap-2">
                <Button type="submit">
                  Select
                </Button>
                <Button type="button" variant="secondary" onClick={handleSignOut}>
                  Sign Out
                </Button>
              </div>
            </form>
          </div>
        </div>
      </div>
    </div>
  );
}
